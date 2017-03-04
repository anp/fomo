use std::collections::BTreeMap;
use std::fs::{DirEntry, Metadata};
use std::path::{Components, Path, PathBuf};

use chrono::{DateTime, Local};
use walkdir;
use walkdir::WalkDir;

use error::*;
use times::system_time_to_date_time;

#[derive(Debug, Eq, PartialEq, Serialize)]
pub struct FsNode {
  path: PathBuf,
  basename: String,
  entry: FsEntryType,
  mtime: DateTime<Local>,
}

#[derive(Debug, Serialize)]
pub struct FsRootNode(FsNode);

impl FsRootNode {
  pub fn new() -> Self {
    FsRootNode(FsNode {
      path: PathBuf::from(""),
      basename: String::new(),
      entry: FsEntryType::Directory { children: BTreeMap::new(), },
      mtime: Local::now(),
    })
  }

  pub fn add_root(&mut self, path: &Path) -> FomoResult<()> {

    for entry in WalkDir::new(path) {
      let entry = entry?;
      let mut parent = {
        let mut components = entry.path().components();
        // we need to remove the final element from the path, as it corresponds to the DirEntry
        components.next_back();
        let path_buffer = PathBuf::new();

        self.0.ensure_and_return_parent(path_buffer, components)?
      };

      {

        let new_node = FsNode::try_from_node_source(entry)?;
        let basename = new_node.basename.to_owned();

        match parent.entry {
          FsEntryType::Directory { ref mut children } => {
            children.insert(basename, new_node);
          }
          _ => panic!("Found a non-directory as the parent of a node."),
        }
      }
    }

    Ok(())
  }
}

impl FsNode {
  /// This should only be called if the node is a root node.
  fn ensure_and_return_parent<'a, 'b>(
    &'a mut self,
    mut path_so_far: PathBuf,
    mut components: Components<'b>
  ) -> FomoResult<&'a mut FsNode> {

    let component = components.next();

    if let Some(c) = component {

      let comp_str = match c.as_os_str().to_str() {
        Some(s) => s,
        None => return Err(FomoError::Internal("non unicode filename found")),
      };
      path_so_far.push(comp_str);

      match self.entry {
        FsEntryType::Directory { ref mut children } => {
          children.entry(comp_str.to_owned())
            .or_insert_with(|| FsNode::empty_dir(path_so_far.clone(), comp_str.to_owned()))
            .ensure_and_return_parent(path_so_far, components)
        }
        _ => panic!("Files should not exist in the middle of a path component."),
      }
    } else {
      Ok(self)
    }
  }

  pub fn empty_dir(path: PathBuf, dirname: String) -> Self {
    FsNode {
      path: path,
      basename: dirname,
      entry: FsEntryType::Directory { children: BTreeMap::new(), },
      mtime: Local::now(),
    }
  }

  pub fn try_from_node_source<S: NodeSource>(entry: S) -> FomoResult<Self> {

    let basename = match entry.path().file_name() {
      Some(n) => {
        match n.to_str() {
          Some(b) => b.to_owned(),
          None => return Err(FomoError::Internal("non-unicode filename")),
        }
      }
      None => return Err(FomoError::Internal("walkdir gave us a relative path")),
    };

    let metadata = entry.metadata()?;

    let entry_ty = if metadata.is_dir() {
      FsEntryType::Directory { children: BTreeMap::new(), }
    } else if metadata.is_file() {
      FsEntryType::File { len: metadata.len(), }
    } else {
      // FIXME(dikaiosune) this doesn't handle links or other entry types
      unreachable!()
    };

    Ok(FsNode {
      path: entry.path(),
      basename: basename,
      entry: entry_ty,
      mtime: system_time_to_date_time(metadata.modified()?),
    })
  }
}

#[derive(Debug, Eq, PartialEq, Serialize)]
pub enum FsEntryType {
  Directory { children: BTreeMap<String, FsNode>, },
  File { len: u64, },
}

pub trait NodeSource {
  fn path(&self) -> PathBuf;
  fn metadata(&self) -> FomoResult<Metadata>;
}

impl NodeSource for walkdir::DirEntry {
  fn path(&self) -> PathBuf {
    self.path().to_path_buf()
  }

  fn metadata(&self) -> FomoResult<Metadata> {
    Ok(self.metadata()?)
  }
}

impl NodeSource for DirEntry {
  fn path(&self) -> PathBuf {
    self.path()
  }

  fn metadata(&self) -> FomoResult<Metadata> {
    Ok(self.metadata()?)
  }
}

#[cfg(test)]
mod test {
  use std::fs::File;
  use std::io::prelude::*;
  use tempdir::TempDir;
  use walkdir::WalkDir;

  use super::{FsEntryType, FsNode};

  #[test]
  fn single_file_tmp_dir() {
    let tmp = TempDir::new("single").expect("unable to create temp directory");
    let tmp_path = tmp.path().join("test-file");
    let mut tmp_file = File::create(&tmp_path).expect("unable to create test file");

    let mut traversed = 0;
    for entry in WalkDir::new(&tmp_path) {
      let entry = entry.expect("walking directory");
      let node = FsNode::try_from_node_source(entry).expect("creating node");
      assert_eq!(node.basename, "test-file");
      assert_eq!(node.path, tmp_path);
      assert_eq!(node.entry, FsEntryType::File { len: 0, });
      traversed += 1;
    }
    assert_eq!(traversed, 1);

    write!(tmp_file, "7 bytes").expect("writing to temp file");

    traversed = 0;
    for entry in WalkDir::new(&tmp_path) {
      let entry = entry.expect("walking directory");
      let node = FsNode::try_from_node_source(entry).expect("creating node");
      assert_eq!(node.basename, "test-file");
      assert_eq!(node.path, tmp_path);
      assert_eq!(node.entry, FsEntryType::File { len: 7, });
      traversed += 1;
    }
    assert_eq!(traversed, 1);
  }
}
