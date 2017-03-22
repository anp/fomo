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
#[macro_use]
extern crate error_chain;
extern crate glob;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate walkdir;

#[cfg(test)]
extern crate tempdir;

use clap::{App, Arg};
use std::path::Path;

// mod command;

mod errors {
  // Create the Error, ErrorKind, ResultExt, and Result types
  use glob::PatternError;
  error_chain!{
    foreign_links {
      GlobError(PatternError);
    }
  }
}

pub mod fs_view;
pub mod query;
mod times;

use errors::*;

fn run() -> Result<()> {
  // TODO print a single JSON item describing the startup & use of a port/socket/etc
  let before_help_msg = "If no flags are specified, a default set of listeners will be selected \
and printed in the startup message.";

  let args = App::new(env!("CARGO_PKG_NAME"))
    .version(env!("CARGO_PKG_VERSION"))
    .author(env!("CARGO_PKG_AUTHORS"))
    .about(env!("CARGO_PKG_DESCRIPTION"))
    .after_help(before_help_msg)
    .arg(Arg::with_name("port")
      .help("Specify a port to use for listening to queries.")
      .short("p")
      .long("port")
      .takes_value(true))
    .arg(Arg::with_name("logfile")
      .help("A path to write log output to.")
      .short("l")
      .long("log-to")
      .takes_value(true))
    .arg(Arg::with_name("root").index(1).required(true))
    .get_matches();

  let root_path = args.value_of("root").unwrap();
  println!("{}", root_path);

  let mut root = fs_view::FsRootNode::new();

  root.add_root(Path::new(root_path)).expect("lol this is broken");

  println!("{}",
           serde_json::to_string_pretty(&root).expect("couldn't serialize"));

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
