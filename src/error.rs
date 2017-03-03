use std::io;

use walkdir;

pub type FomoResult<T> = Result<T, FomoError>;

#[derive(Debug)]
pub enum FomoError {
  Io(io::Error),
  Walkdir(walkdir::Error),
  Internal(&'static str),
}

impl From<io::Error> for FomoError {
  fn from(e: io::Error) -> FomoError {
    FomoError::Io(e)
  }
}

impl From<walkdir::Error> for FomoError {
  fn from(e: walkdir::Error) -> FomoError {
    FomoError::Walkdir(e)
  }
}
