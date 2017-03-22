use std;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::fs::{DirEntry, read_link};
use std::path::{Components, Path, PathBuf};

use chrono::{DateTime, Local};
use walkdir;
use walkdir::WalkDir;

use errors::*;
use times::system_time_to_date_time;

#[cfg(test)]
use std::fs::{OpenOptions, create_dir_all};

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct FsNode {
  pub path: PathBuf,
  pub basename: String,
  pub entry: FsEntryType,
  pub mtime: DateTime<Local>,
}

#[derive(Debug, Serialize)]
pub struct FsRootNode(FsNode);

impl<'a> FsRootNode {
  pub fn iter(&'a self) -> FsIterator<'a> {
    FsIterator {
      to_visit: Vec::new(),
      current: Some(&self.0),
    }
  }
}

impl<'a> IntoIterator for &'a FsRootNode {
  type Item = &'a FsNode;
  type IntoIter = FsIterator<'a>;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

pub struct FsIterator<'a> {
  to_visit: Vec<&'a FsNode>,
  current: Option<&'a FsNode>,
}

impl<'a> Iterator for FsIterator<'a> {
  type Item = &'a FsNode;

  fn next(&mut self) -> Option<Self::Item> {
    // if the current node is a dir, add all to to_visit
    match self.current {
      Some(ref node) => {
        // add children to the stack to visit next
        match node.entry {
          FsEntryType::RootRoot { ref children } |
          FsEntryType::Directory { ref children } => {
            self.to_visit.extend(children.values().rev());
          }
          FsEntryType::File { .. } |
          FsEntryType::Symlink { .. } => (),
        }
      }
      None => (),
    };

    // set current to the next to_visit item

    // return the previous current
    let returned = self.current;

    self.current = self.to_visit.pop();

    returned
  }
}

impl FsRootNode {
  pub fn new() -> Self {
    FsRootNode(FsNode {
      path: PathBuf::from(""),
      basename: String::new(),
      entry: FsEntryType::RootRoot { children: BTreeMap::new(), },
      mtime: Local::now(),
    })
  }

  pub fn add_root(&mut self, path: &Path) -> Result<()> {

    for entry in WalkDir::new(path) {
      let entry = entry.chain_err(|| "unable to fetch walkdir entry")?;
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
  ) -> Result<&'a mut FsNode> {

    let component = components.next();

    if let Some(c) = component {

      let comp_str = match c.as_os_str().to_str() {
        Some(s) => s,
        None => bail!("non unicode filename found"),
      };
      path_so_far.push(comp_str);

      match self.entry {
        FsEntryType::Directory { ref mut children } |
        FsEntryType::RootRoot { ref mut children } => {
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

  pub fn try_from_node_source<S: NodeSource>(entry: S) -> Result<Self> {

    let basename = match entry.path().file_name() {
      Some(n) => {
        match n.to_str() {
          Some(b) => b.to_owned(),
          None => bail!("non-unicode filename"),
        }
      }
      None => bail!("walkdir gave us a relative path"),
    };

    let metadata = entry.metadata()?;

    let file_type = metadata.ty;
    let entry_ty = match file_type {
      FsItemType::Directory => FsEntryType::Directory { children: BTreeMap::new(), },
      FsItemType::File => FsEntryType::File { len: metadata.len, },
      FsItemType::SymlinkUgh => {
        let sym_path = read_link(entry.path()).chain_err(|| "unable to read through symlink")?;
        let sym_meta = sym_path.metadata()
          .chain_err(|| format!("unable to read symlink target's ({:?}) metadata", sym_path))?;
        let sym_target_type = if sym_meta.file_type().is_file() {
          FsItemType::File
        } else if sym_meta.file_type().is_dir() {
          FsItemType::Directory
        } else if sym_meta.file_type().is_symlink() {
          FsItemType::SymlinkUgh
        } else {
          FsItemType::Other
        };

        FsEntryType::Symlink {
          target: sym_path,
          ty: sym_target_type,
        }
      }
      FsItemType::Other => {
        panic!("This is only ever used when reading symlink targets, this is a bug.");
      }
    };

    Ok(FsNode {
      path: entry.path(),
      basename: basename,
      entry: entry_ty,
      mtime: metadata.mtime,
    })
  }

  fn format_into_buffer(&self, buf: &mut String, depth: u32) {
    for _ in 0..(depth * 2) {
      buf.push(' ');
    }

    buf.push_str(&self.basename);

    match &self.entry {
      &FsEntryType::RootRoot { ref children } |
      &FsEntryType::Directory { ref children } => {
        buf.push('\n');
        for (_, child) in children {
          child.format_into_buffer(buf, depth + 1);
        }
      }
      &FsEntryType::File { len } => {
        let node_md_str = format!(" {} {}\n", len, self.mtime);
        buf.push_str(&node_md_str);
      }
      &FsEntryType::Symlink { ref target, ty } => {
        let node_md_str = format!(" -> {:?} ({:?})\n", target, ty);
        buf.push_str(&node_md_str);
      }
    }
  }
}

impl Display for FsNode {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
    let mut buf = String::new();

    self.format_into_buffer(&mut buf, 0);

    let _ = f.write_str(&buf);

    Ok(())
  }
}

#[cfg(all(test, not(windows)))]
#[allow(unused_variables)]
fn make_symlink(src: &Path, dst: &Path, ty: FsItemType) -> Result<()> {
  use std::os::unix::fs::symlink;
  Ok(symlink(src, dst).chain_err(|| "unable to create symlink")?)
}

#[cfg(all(test, windows))]
fn make_symlink(src: &Path, dst: &Path, ty: FsItemType) -> Result<()> {
  use std::os::windows::fs::{symlink_dir, symlink_file};
  match ty {
    FsItemType::Directory => {
      Ok(symlink_dir(src, dst).chain_err(|| "unable to create directory symlink")?)
    }
    FsItemType::File | FsItemType::SymlinkUgh => {
      Ok(symlink_file(src, dst).chain_err(|| "unable to create file symlink")?)
    }
    // we don't want to try to write any of this stuff to disk
    FsItemType::Other => Ok(()),
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub enum FsEntryType {
  Directory { children: BTreeMap<String, FsNode>, },
  File { len: u64, },
  Symlink { target: PathBuf, ty: FsItemType, },
  // different than a root path element (thanks, windows)
  RootRoot { children: BTreeMap<String, FsNode>, },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub enum FsItemType {
  File,
  Directory,
  SymlinkUgh,
  Other,
}

pub struct MetadataFromFs {
  ty: FsItemType,
  mtime: DateTime<Local>,
  len: u64,
}

pub trait NodeSource {
  fn path(&self) -> PathBuf;
  fn metadata(&self) -> Result<MetadataFromFs>;
}

impl NodeSource for walkdir::DirEntry {
  fn path(&self) -> PathBuf {
    self.path().to_path_buf()
  }

  fn metadata(&self) -> Result<MetadataFromFs> {
    let md = self.metadata().chain_err(|| "unable to read file metadata")?;
    let mtime = system_time_to_date_time(md.modified()
      .chain_err(|| "unable to read modification time")?);


    Ok(MetadataFromFs {
      mtime: mtime,
      len: md.len(),
      ty: if md.is_file() {
        FsItemType::File
      } else if md.is_dir() {
        FsItemType::Directory
      } else {
        FsItemType::SymlinkUgh
      },
    })
  }
}

impl NodeSource for DirEntry {
  fn path(&self) -> PathBuf {
    self.path()
  }

  fn metadata(&self) -> Result<MetadataFromFs> {
    let md = self.metadata().chain_err(|| "unable to read file metadata")?;
    let mtime = system_time_to_date_time(md.modified()
      .chain_err(|| "unable to read file modified time")?);

    Ok(MetadataFromFs {
      mtime: mtime,
      len: md.len(),
      ty: if md.is_file() {
        FsItemType::File
      } else if md.is_dir() {
        FsItemType::Directory
      } else {
        FsItemType::SymlinkUgh
      },
    })
  }
}

#[cfg(test)]
mod test {
  use chrono::Local;
  use std::fs::File;
  use std::io::prelude::*;
  use std::path::PathBuf;
  use tempdir::TempDir;
  use walkdir::WalkDir;

  use super::*;

  impl FsRootNode {
    pub fn insert_node(&mut self, node: FsNode) -> Result<()> {
      let basename = node.basename.clone();
      let path = node.path.clone();
      let mut components = path.components();
      components.next_back();
      match self.0
        .ensure_and_return_parent(PathBuf::new(), components)
        .chain_err(|| "unable to ensure parent for new node insertion")?
        .entry {

        FsEntryType::RootRoot { ref mut children } |
        FsEntryType::Directory { ref mut children } => children.insert(basename, node),
        _ => panic!("Found a non-directory node when ensuring parent of a node was in the tree"),
      };
      Ok(())
    }
  }

  impl FsNode {
    /// WARNING: Only call this in tests when you've carefully constructed a tree inside a
    /// temporary directory. Potentially destructive, and not easily reversed.
    ///
    /// Also, requires that the process has `SeCreateSymbolicLinkPrivilege`.
    fn mirror_to_disk(&self) -> Result<()> {
      match self.entry {
        FsEntryType::RootRoot { ref children } => {
          for child in children.values() {
            child.mirror_to_disk()?;
          }
        }
        FsEntryType::Directory { ref children } => {
          create_dir_all(&self.path).chain_err(|| "unable to recursively create directories")?;

          for (_, child) in children {
            child.mirror_to_disk()?
          }
        }
        FsEntryType::File { len } => {
          println!("attempting to create file {:?}", &self.path);
          let file = OpenOptions::new().read(true)
            .write(true)
            .create(true)
            .open(&self.path)
            .chain_err(|| "unable to open/create file")?;
          file.set_len(len).chain_err(|| "unable to set file length")?;
        }
        FsEntryType::Symlink { ref target, ty } => {
          make_symlink(target, &self.path, ty)?;
        }
      }
      Ok(())
    }

    fn assert_eq_with_mtime_epsilon(&self, other: &FsNode, acceptable_time_gap_millis: i64) {
      if self.path != other.path {
        panic!("paths are not equal\nlhs: {:?}\nrhs: {:?}", self, other);
      }

      if self.basename != other.basename {
        panic!("basenames are not equal\nlhs: {:?}\nrhs: {:?}", self, other);
      }

      if self.mtime.signed_duration_since(other.mtime).num_milliseconds().abs() >
         acceptable_time_gap_millis {
        panic!("mtimes are not equal (w/in {:?} tolerance): {} and {}\nlhs:{:?}\nrhs:{:?}",
               acceptable_time_gap_millis,
               self.mtime,
               other.mtime,
               self,
               other);
      }

      match (&self.entry, &other.entry) {
        (&FsEntryType::File { len: len1 }, &FsEntryType::File { len: len2 }) => {
          if len1 != len2 {
            panic!("file lengths are not equal\nlhs: {:?}\nrhs: {:?}",
                   self,
                   other);
          }
        }
        (&FsEntryType::RootRoot { children: ref children1 },
         &FsEntryType::RootRoot { children: ref children2 }) |
        (&FsEntryType::Directory { children: ref children1 },
         &FsEntryType::Directory { children: ref children2 }) => {

          // these are btreemaps so will have the same sort order
          for ((basename1, child1), (basename2, child2)) in children1.iter().zip(children2.iter()) {
            if basename1 != basename2 {
              panic!("children basenames are not equal\nlhs: {:?}\nrhs: {:?}",
                     child1,
                     child2);
            }
            child1.assert_eq_with_mtime_epsilon(child2, acceptable_time_gap_millis);
          }
        }

        (&FsEntryType::Symlink { target: ref target1, ty: ty1 },
         &FsEntryType::Symlink { target: ref target2, ty: ty2 }) => {
          if ty1 != ty2 {
            panic!("symlink types are not equal\nlhs: {:?}\nrhs: {:?}",
                   self,
                   other);
          }

          if target1 != target2 {
            panic!("symlink targets are not equal\nlhs: {:?}\nrhs: {:?}",
                   self,
                   other);
          }
        }
        _ => {
          panic!("node entry types are not equal\nlhs: {:?}\nrhs: {:?}",
                 self,
                 other)
        }
      }
    }
  }

  #[cfg(not(windows))]
  mod symlink_target_paths {
    pub const FILE: &'static str = "/bin/bash";
    pub const DIRECTORY: &'static str = "/usr";
  }

  #[cfg(windows)]
  mod symlink_target_paths {
    #[allow(dead_code)]
    pub const FILE: &'static str = "C:\\Windows\\explorer.exe";
    #[allow(dead_code)]
    pub const DIRECTORY: &'static str = "C:\\Windows";
  }

  #[test]
  fn fixed_roundtrip() {
    let start_time = Local::now();
    let tmp = TempDir::new("mirroring").expect("couldn't create temp dir");
    // make the fake filesystem, keeping a list of paths created
    let many_child_dir_path = tmp.path().join("many_child_dir");
    let single_child_dir_path = many_child_dir_path.join("single_child_dir");
    let empty_dir_path = many_child_dir_path.join("empty_dir");

    let empty_file_path = single_child_dir_path.join("empty_file");
    let single_byte_file_path = many_child_dir_path.join("single_byte_file");
    let twenty_two_byte_file_path = many_child_dir_path.join("twenty_two_byte_file");

    let symlinks_path = many_child_dir_path.join("symlinks");
    let symlink_to_file_path = symlinks_path.join("symlink_to_file");
    let symlink_to_dir_path = symlinks_path.join("symlink_to_dir");

    let to_find = {
      let mut to_find = vec![ tmp.path().to_path_buf(),
                              many_child_dir_path.clone(),
                              single_child_dir_path.clone(),
                              empty_dir_path.clone(),
                              empty_file_path.clone(),
                              single_byte_file_path.clone(),
                              twenty_two_byte_file_path.clone() ];

      #[cfg(not(windows))]
      {
        to_find.push(symlinks_path);
        to_find.push(symlink_to_file_path.clone());
        to_find.push(symlink_to_dir_path.clone());
      }
      to_find.sort();
      to_find
    };

    let mut root_node = FsRootNode::new();

    root_node.insert_node(FsNode {
        path: many_child_dir_path,
        basename: String::from("many_child_dir"),
        entry: FsEntryType::Directory { children: BTreeMap::new(), },
        mtime: Local::now(),
      })
      .expect("couldn't insert many_child_dir");

    root_node.insert_node(FsNode {
        path: single_child_dir_path,
        basename: String::from("single_child_dir"),
        entry: FsEntryType::Directory { children: BTreeMap::new(), },
        mtime: Local::now(),
      })
      .expect("couldn't insert single_child_dir");

    root_node.insert_node(FsNode {
        path: empty_dir_path,
        basename: String::from("empty_dir"),
        entry: FsEntryType::Directory { children: BTreeMap::new(), },
        mtime: Local::now(),
      })
      .expect("couldn't insert empty_dir");

    root_node.insert_node(FsNode {
        path: empty_file_path,
        basename: String::from("empty_file"),
        entry: FsEntryType::File { len: 0, },
        mtime: Local::now(),
      })
      .expect("couldn't insert empty_file");

    root_node.insert_node(FsNode {
        path: single_byte_file_path,
        basename: String::from("single_byte_file"),
        entry: FsEntryType::File { len: 1, },
        mtime: Local::now(),
      })
      .expect("couldn't insert single_byte_file");

    root_node.insert_node(FsNode {
        path: twenty_two_byte_file_path,
        basename: String::from("twenty_two_byte_file"),
        entry: FsEntryType::File { len: 22, },
        mtime: Local::now(),
      })
      .expect("couldn't insert twenty_two_byte_file");

    #[cfg(not(windows))]
    {
      root_node.insert_node(FsNode {
          path: symlink_to_file_path,
          basename: String::from("symlink_to_file"),
          entry: FsEntryType::Symlink {
            target: PathBuf::from(symlink_target_paths::FILE),
            ty: FsItemType::File,
          },
          mtime: Local::now(),
        })
        .expect("couldn't insert symlink_to_file");

      root_node.insert_node(FsNode {
          path: symlink_to_dir_path,
          basename: String::from("symlink_to_dir"),
          entry: FsEntryType::Symlink {
            target: PathBuf::from(symlink_target_paths::DIRECTORY),
            ty: FsItemType::Directory,
          },
          mtime: Local::now(),
        })
        .expect("couldn't insert symlink_to_dir");
    }

    // write to disk
    let res = root_node.0.mirror_to_disk();
    match res {
      Ok(_) => (),
      Err(why) => {
        println!("{:#?}", root_node);
        panic!("unable to write tree to disk: {:?}", why);
      }
    }

    // figure there's maybe a second of wobble on either side in addition to however slowly
    // this test has run so far
    let acceptable_epsilon =
      Local::now().signed_duration_since(start_time).num_milliseconds().abs() + 1000;

    // assemble list of paths from walkdir
    let mut found_items = WalkDir::new(tmp.path())
      .into_iter()
      .map(|r| r.expect("unable to read from walkdir iterator").path().to_path_buf())
      .collect::<Vec<_>>();

    let iteration_order = root_node.iter()
      .filter(|n| n.path.starts_with(tmp.path()))
      .map(|n| n.path.clone())
      .collect::<Vec<_>>();

    found_items.sort();
    assert_eq!(iteration_order, found_items);
    assert_eq!(to_find, found_items);

    println!("found files: {:#?}", found_items);

    // construct a fsrootnode, see if we round-tripped correctly
    let mut second_root_node = FsRootNode::new();
    second_root_node.add_root(tmp.path()).expect("unable to construct pair fs view");

    println!("root: {}\n\nsecond_root: {}",
             root_node.0,
             second_root_node.0);

    root_node.0.assert_eq_with_mtime_epsilon(&second_root_node.0, acceptable_epsilon)
  }

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
