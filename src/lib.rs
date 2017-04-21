//! `fomo` is an experiment to see if the file watching needs of build tools can
//! be solved a bit differently than existing solutions.
//!
//! For example, solutions like [chokidar](TODOLINKHERE) are often unable to
//! keep up with sweeping changes like `rm -rf node_modules`. Other solutions
//! like [watchman](TODOLINKHERE) are fantastic for many tasks, but also have several
//! layers of caching which sometimes must be manually deleted due to corrupt
//! data.
//!
//! So anyways, the main goal here is to see if we can find a goldilocks for
//! the needs of file watching for JavaScript tools (to start).


//#![warn(missing_docs)]

// error-chain can go pretty deep
#![recursion_limit = "1024"]

extern crate chrono;
extern crate crossbeam;
#[macro_use]
extern crate error_chain;
extern crate glob;
#[macro_use]
extern crate log;
extern crate regex;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate walkdir;

#[macro_use]
extern crate bitflags;
#[cfg(target_os="linux")]
extern crate mio;
#[cfg(target_os="macos")]
extern crate fsevent_sys;
#[cfg(target_os="windows")]
extern crate winapi;
extern crate libc;
extern crate filetime;

#[cfg(test)]
extern crate tempdir;

use std::io::prelude::*;
use std::sync::mpsc::channel;
use std::time::Duration;

use notify::{RecursiveMode, Watcher};

pub mod errors {
  // Create the Error, ErrorKind, ResultExt, and Result types
  error_chain!{
    foreign_links {
      GlobError(::glob::PatternError);
      JsonError(::serde_json::Error);
      IoError(::std::io::Error);
      StdoutMpscError(::std::sync::mpsc::SendError<String>);
      WalkdirError(::walkdir::Error);
    }
  }
}

pub mod fs_view;
mod glob_wrapper;
pub mod notify;
pub mod query;
mod regex_wrapper;
mod times;

use errors::*;

#[derive(Debug)]
pub enum RootMessage {
  Query(query::QueryError, query::Query),
  Event(notify::DebouncedEvent),
}

pub fn run_for_realsies<R>(stdin: R) -> Result<()>
  where R: Read + BufRead
{
  crossbeam::scope(|scope| {
    let (stdout_tx, stdout_rx) = channel();
    let (root_tx, root_rx) = channel();

    scope.spawn(|| {
      let stdout = ::std::io::stdout();
      let mut stdout = stdout.lock();
      for l in stdout_rx {
        match writeln!(&mut stdout, "{}", l) {
          Ok(_) => (),
          Err(why) => {
            // if this happens there's no way you're getting file events from the kernel
            error!("can't write to stdout: {:?}", why);
            ::std::process::exit(1);
          }
        }
      }
    });

    // spawn the fs root thread
    let fs_stdout_tx = stdout_tx.clone();
    let watcher_root_tx = root_tx.clone();
    scope.spawn(move || {
      let mut fs = fs_view::FsRootNode::new();
      let mut watcher = match notify::watcher(watcher_root_tx, Duration::from_millis(300)) {
        Ok(w) => w,
        Err(why) => {
          error!("Unable to start file watcher: {:?}", why);
          ::std::process::exit(1);
        }
      };

      // ok we finally have a valid query
      for msg in root_rx {
        match msg {
          RootMessage::Event(event) => {
            match fs.consume_event(event) {
              Ok(Some(notifs)) => {
                for notif in notifs {
                  match serde_json::to_string(&notif) {
                    Ok(s) => {
                      match fs_stdout_tx.send(s) {
                        Ok(_) => (),
                        Err(why) => {
                          error!("mpsc send failed! {:?}", why);
                          ::std::process::exit(1);
                        }
                      }
                    }
                    Err(why) => {
                      error!("unable to encode notification ({:?}) as JSON: {:?}",
                             notif,
                             why);
                      ::std::process::exit(1);
                    }
                  }
                }
              }
              Ok(None) => (),
              Err(why) => {
                error!("unable to consume event: {:?}", why);
                // FIXME notify the client of this somehow
              }
            }
          }
          RootMessage::Query(mut error, query) => {
            let to_watch = if query.watch {
              Some(query.root.clone())
            } else {
              None
            };

            let res = match fs.eval(query) {
              Ok(r) => {
                if let Some(watch_root) = to_watch {
                  match watcher.watch(&watch_root, RecursiveMode::Recursive) {
                    Ok(_) => (),
                    Err(why) => {
                      error!("unable to watch root {:?}: {:?}", watch_root, why);
                      ::std::process::exit(1);
                    }
                  }
                }
                r
              }
              Err(why) => {
                error!("Unable to evaluate query: {:?}", why);
                error.error = Some("eval".to_string());
                error.human_error = Some(format!("Problem evaluating query: {:?}", why));

                match serde_json::to_string(&error) {
                  Ok(s) => {
                    match fs_stdout_tx.send(s) {
                      Ok(_) => (),
                      Err(why) => {
                        error!("yeah this should never happen. unable to send mpsc msg: {:?}",
                               why)
                      }
                    }
                  }
                  Err(why) => {
                    error!("unable to serialize value to JSON, this is definitely a bug: {:?}",
                           why)
                  }
                }
                continue;
              }
            };

            match serde_json::to_string(&res) {
              Ok(s) => {
                match fs_stdout_tx.send(s) {
                  Ok(_) => (),
                  Err(why) => {
                    error!("mpsc send failed inexplicably: {:?}", why);
                    ::std::process::exit(1);
                  }
                }
              }
              Err(why) => {
                error!("unable to serialize a definitely-serializable value: {:?}",
                       why);
                ::std::process::exit(1);
              }

            }
          }
        }
      }
    });

    info!("listening to stdin for queries");
    let err_stdout_tx = stdout_tx.clone();
    for input in stdin.lines() {
      let input = input.chain_err(|| "reading from stdin")?;

      // build this incrementally at each stage in case we need to send it back
      let mut error = query::QueryError {
        id: None,
        error: None,
        human_error: None,
        query_string: input.clone(),
      };

      let query: query::Query = match serde_json::from_str(&input) {
        Ok(q) => q,
        Err(why) => {

          error!("Parsing query failed: {:?}. Query string: '{}'", why, input);
          error.error = Some("parse".to_string());

          // let's see if we can find the id of the query at least
          match serde_json::from_str::<query::PartialQuery>(&input) {
            Ok(p) => {
              error.id = Some(p.id);
              error.human_error = Some(format!("Unable to parse query object: {:?}", why));
            }
            Err(why) => {
              error!("Unable to retrieve id from query string: {:?}", why);
              error.human_error = Some(format!("Unable to parse query or even id from query \
                                                object: {:?}",
                                               why));
            }
          }

          err_stdout_tx.send(serde_json::to_string(&error)?)?;
          continue;
        }
      };

      error.id = Some(query.id.clone());

      match root_tx.send(RootMessage::Query(error, query)) {
        Ok(_) => (),
        Err(why) => {
          error!("mpsc send failed: {:?}", why);
          ::std::process::exit(1);
        }
      }
    }
    Ok(())
  })
}
