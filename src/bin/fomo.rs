#![recursion_limit = "1024"]

extern crate clap;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate fomo;
#[macro_use]
extern crate log;

use clap::App;
use fomo::errors::*;

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

  fomo::run_for_realsies(stdin)
}

// TODO(dikaiosune) write a catch_panic facade that restarts everything
quick_main!(run);
