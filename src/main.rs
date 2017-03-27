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
extern crate clap;
extern crate env_logger;
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

#[cfg(test)]
extern crate tempdir;

use std::collections::BTreeMap;
use std::io::prelude::*;

use clap::App;

mod errors {
  // Create the Error, ErrorKind, ResultExt, and Result types
  error_chain!{
    foreign_links {
      GlobError(::glob::PatternError);
      JsonError(::serde_json::Error);
      IoError(::std::io::Error);
    }
  }
}

pub mod fs_view;
mod glob_wrapper;
pub mod query;
mod regex_wrapper;
mod times;

use errors::*;

fn run() -> Result<()> {
  env_logger::LogBuilder::new()
    .format(|rec: &log::LogRecord| format!("{}:{}", rec.level(), rec.args()))
    .filter(None, log::LogLevelFilter::Debug)
    .init()
    .unwrap();

  // don't need any arguments, as they should be passed by env var
  // but we want a nice help message for them
  // TODO write help message about env vars
  App::new(env!("CARGO_PKG_NAME"))
    .version(env!("CARGO_PKG_VERSION"))
    .author(env!("CARGO_PKG_AUTHORS"))
    .about(env!("CARGO_PKG_DESCRIPTION"))
    .before_help("Logging can be configured via the RUST_LOG environment variable.
See https://docs.rs/env_logger/ for details.")
    .get_matches();

  // acquire exclusive use of stdin/stdout
  info!("Initializing stdin/stdout handles");
  let stdin = ::std::io::stdin();
  let stdin = stdin.lock();
  let stdout = ::std::io::stdout();
  let stdout = stdout.lock();

  run_for_realsies(stdin, stdout)
}

const ERROR_TYPE_KEY: &'static str = "error";
const HUMAN_ERROR_KEY: &'static str = "humanError";
const QUERY_STRING_KEY: &'static str = "queryString";
const ID_KEY: &'static str = "id";

fn run_for_realsies<R, W>(stdin: R, mut stdout: W) -> Result<()>
  where R: Read + BufRead,
        W: Write
{
  let mut fs = fs_view::FsRootNode::new();

  info!("listening to stdin for queries");
  for input in stdin.lines() {
    let input = input.chain_err(|| "reading from stdin")?;

    // build this incrementally at each stage in case we need to send it back
    let mut error_map = BTreeMap::new();
    error_map.insert(QUERY_STRING_KEY, input.clone());

    let query: query::Query = match serde_json::from_str(&input) {
      Ok(q) => q,
      Err(why) => {

        error!("Parsing query failed: {:?}. Query string: '{}'", why, input);
        error_map.insert(ERROR_TYPE_KEY, "parse".to_string());

        // let's see if we can find the id of the query at least
        match serde_json::from_str::<query::PartialQuery>(&input) {
          Ok(p) => {
            error_map.insert(ID_KEY, p.id);
            error_map.insert(HUMAN_ERROR_KEY,
                             format!("Unable to parse query object: {:?}", why));
          }
          Err(why) => {
            error!("Unable to retrieve id from query string: {:?}", why);
            error_map.insert(HUMAN_ERROR_KEY,
                             format!("Unable to parse query or even id from query object: {:?}",
                                     why));
          }
        }

        writeln!(&mut stdout, "{}", serde_json::to_string(&error_map)?)?;
        continue;
      }
    };

    error_map.insert(ID_KEY, query.id.clone());

    // ok we finally have a valid query
    let res = match fs.eval(query) {
      Ok(r) => r,
      Err(why) => {
        error!("Unable to evaluate query: {:?}", why);
        error_map.insert(ERROR_TYPE_KEY, "eval".to_string());
        error_map.insert(HUMAN_ERROR_KEY,
                         format!("Problem evaluating query: {:?}", why));

        writeln!(&mut stdout, "{}", serde_json::to_string(&error_map)?)?;
        continue;
      }
    };

    writeln!(&mut stdout, "{}", serde_json::to_string(&res)?)?;
  }
  Ok(())
}

// TODO(dikaiosune) write a catch_panic facade that restarts everything
quick_main!(run);

#[derive(Deserialize, Serialize)]
struct StartupMessage {
  sync_port: u16,
  ws_port: u16,
  error: Option<StartError>,
  error_human: String,
}

#[derive(Deserialize, Serialize)]
enum StartError {
  PortUnavailable,
  PlatformUnsupported,
}
