use std;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::fs::{DirEntry, metadata, read_link};
use std::path::{Components, Path, PathBuf};

use chrono::{DateTime, Local, UTC};
use walkdir;
use walkdir::WalkDir;

use errors::*;
use notify::DebouncedEvent;
use query::{FileResult, Query, QueryExpression, QueryResult};
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

#[derive(Clone, Copy, Debug, Serialize)]
pub enum ChangeEvent {
  Create,
  Delete,
  Write,
  Metadata,
}

#[derive(Debug, Serialize)]
pub struct FileEvent {
  event: ChangeEvent,
  file: FileResult,
}

#[derive(Debug, Serialize)]
pub struct Notification {
  changes: Vec<FileEvent>,
  root: PathBuf,
}

#[derive(Debug, Serialize)]
pub struct FsRootNode {
  base: FsNode,
  roots: BTreeMap<PathBuf, QueryExpression>,
}

impl<'a> FsRootNode {
  pub fn iter(&'a self) -> FsIterator<'a> {
    FsIterator {
      to_visit: Vec::new(),
      current: Some(&self.base),
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

    // return the previous current
    let returned = self.current;

    self.current = self.to_visit.pop();

    returned
  }
}

impl FsRootNode {
  pub fn new() -> Self {
    FsRootNode {
      base: FsNode {
        path: PathBuf::from(""),
        basename: String::new(),
        entry: FsEntryType::RootRoot { children: BTreeMap::new(), },
        mtime: Local::now(),
      },
      roots: BTreeMap::new(),
    }
  }

  fn insert_node(&mut self, node: FsNode) -> Result<()> {
    let basename = node.basename.clone();
    let path = node.path.clone();
    let mut components = path.components();
    components.next_back();
    match self.base
      .ensure_and_return_parent(PathBuf::new(), components)
      .chain_err(|| "unable to ensure parent for new node insertion")?
      .entry {

      FsEntryType::RootRoot { ref mut children } |
      FsEntryType::Directory { ref mut children } => children.insert(basename, node),
      _ => bail!("Found a non-directory node when ensuring parent of a node was in the tree"),
    };
    Ok(())
  }

  fn remove_node(&mut self, path: &Path) -> Option<FsNode> {
    let mut components = path.components();
    let basename = match components.next_back() {
      Some(b) => b.as_os_str().to_string_lossy(),
      None => return None,
    };

    if let Ok(parent) = self.base.ensure_and_return_parent(PathBuf::new(), components) {
      match parent.entry {
        FsEntryType::RootRoot { ref mut children } |
        FsEntryType::Directory { ref mut children } => children.remove(&*basename),
        _ => None,
      }
    } else {
      None
    }
  }

  fn needs_notification(&self, node: &FsNode) -> bool {
    for (ref root, expr) in &self.roots {
      if node.path.starts_with(root) && expr.matches(&node) {
        return true;
      }
    }
    false
  }

  pub fn consume_event(&mut self, event: DebouncedEvent) -> Result<Option<Notification>> {
    let mut changes = Vec::new();
    let event_root;

    fn find_matching_root(
      p: &PathBuf,
      roots: &BTreeMap<PathBuf, QueryExpression>
    ) -> Result<PathBuf> {
      match roots.iter().map(|t| t.0).filter(|r| p.starts_with(r)).next() {
        Some(r) => Ok(r.clone()),
        None => {
          bail!("lol adam is stupid, couldn't find {:?} in roots {:?}",
                &p,
                roots)
        }
      }
    }

    match event {
      // TODO decide how to handle I/O notices vs. confirmations
      DebouncedEvent::NoticeWrite(_) => return Ok(None),
      DebouncedEvent::NoticeRemove(_) => return Ok(None),

      DebouncedEvent::Create(ref p) => {
        let node = FsNode::try_from_node_source(&p).chain_err(|| "constructing node from path")?;

        if self.needs_notification(&node) {
          changes.push(FileEvent {
            event: ChangeEvent::Create,
            file: FileResult::make(&node),
          });
        }

        self.insert_node(node)?;
        event_root = find_matching_root(p, &self.roots)?;
      }

      DebouncedEvent::Write(ref p) => {
        let node = FsNode::try_from_node_source(&p).chain_err(|| "constructing node from path")?;

        if self.needs_notification(&node) {
          changes.push(FileEvent {
            event: ChangeEvent::Write,
            file: FileResult::make(&node),
          });
        }

        self.insert_node(node)?;
        event_root = find_matching_root(p, &self.roots)?;
      }

      DebouncedEvent::Chmod(ref p) => {
        let node = FsNode::try_from_node_source(&p).chain_err(|| "constructing node from path")?;

        if self.needs_notification(&node) {
          changes.push(FileEvent {
            event: ChangeEvent::Metadata,
            file: FileResult::make(&node),
          });
        }

        self.insert_node(node)?;
        event_root = find_matching_root(p, &self.roots)?;
      }

      DebouncedEvent::Remove(ref p) => {

        if let Some(old_node) = self.remove_node(p) {
          if self.needs_notification(&old_node) {
            changes.push(FileEvent {
              event: ChangeEvent::Delete,
              file: FileResult::make(&old_node),
            });
          }
        }

        event_root = find_matching_root(p, &self.roots)?;
      }

      DebouncedEvent::Rename(ref p, ref to) => {
        let new_node =
          FsNode::try_from_node_source(&to).chain_err(|| "constructing node from path")?;

        if self.needs_notification(&new_node) {
          changes.push(FileEvent {
            event: ChangeEvent::Create,
            file: FileResult::make(&new_node),
          });
        }

        self.insert_node(new_node).unwrap();

        if let Some(old_node) = self.remove_node(p) {
          if self.needs_notification(&old_node) {
            changes.push(FileEvent {
              event: ChangeEvent::Delete,
              file: FileResult::make(&old_node),
            });
          }
        }

        event_root = find_matching_root(p, &self.roots)?;
      }

      DebouncedEvent::Rescan => {
        // ok so the old tree is invalid! yay!

        // build a new tree
        let mut new_fake_root = FsRootNode::new();

        for ref root in self.roots.keys() {
          new_fake_root.add_root(root)?;
        }

        // and make a best effort to notify our client of changes there
        self.base.diff(&new_fake_root.base, &mut changes);
        self.base = new_fake_root.base;

        // TODO segment this into multiple notifications, one per root?

        event_root = PathBuf::new();
      }

      DebouncedEvent::Error(e, opt_p) => {
        bail!("notification error at path {:?}: {:?}", opt_p, e);
      }
    }

    Ok(Some(Notification {
      changes: changes,
      root: event_root,
    }))
  }

  pub fn eval(&mut self, query: Query) -> Result<QueryResult> {
    // make sure all the files we care about are in the tree

    if !self.roots.contains_key(&query.root) {
      let start = UTC::now();
      info!("Adding {:?} root to file system view...", &query.root);
      self.add_root(&query.root)?;

      let duration = UTC::now().signed_duration_since(start);
      info!("Added {:?} to file system view, took {} seconds.",
            &query.root,
            duration);
    }

    // we only have one client right now, so the filesystem view should have very low overhead
    // compared to what watchman does, so i don't think we need generators
    let files = self.iter()
      .filter(|n| query.expr.matches(n))
      .map(|n| FileResult::make(n))
      .collect::<Vec<_>>();

    self.roots.insert(query.root, query.expr);

    Ok(QueryResult {
      id: query.id,
      files: files,
    })
  }

  pub fn add_root(&mut self, path: &Path) -> Result<()> {
    WalkDir::new(path).into_iter()
      .map(|entry| {
        entry.map(|f| -> Result<()> {
          let node = FsNode::try_from_node_source(&f)?;
          let mut components = f.path().components();
          // we need to remove the final element from the path, as it corresponds to the DirEntry
          components.next_back();

          let parent = self.base.ensure_and_return_parent(PathBuf::new(), components)?;

          match parent.entry {
            FsEntryType::Directory { ref mut children } => {
              let basename = node.basename.to_owned();
              children.insert(basename, node);
            }
            _ => bail!("Found a non-directory as the parent of a node."),
          }
          Ok(())
        })
      })
      .collect::<::std::result::Result<Vec<_>, _>>()?;

    Ok(())
  }
}

impl FsNode {
  /// This should only be called if the node is a root node.
  // TODO move this to FsRootNode, so this can be statically enforced.
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
        _ => bail!("Files should not exist in the middle of a path component."),
      }
    } else {
      Ok(self)
    }
  }

  pub fn diff(&self, new_node: &FsNode, results: &mut Vec<FileEvent>) {
    if self.path != new_node.path {
      panic!("bug: called diff on nodes without equal paths");
    }

    if self.basename != new_node.basename {
      panic!("bug: called diff on nodes without equal basenames");
    }

    // we'll ignore mtime at this level, because it only really matters for files

    match (&self.entry, &new_node.entry) {
      (&FsEntryType::RootRoot { children: ref old_children },
       &FsEntryType::RootRoot { children: ref new_children }) |
      (&FsEntryType::Directory { children: ref old_children },
       &FsEntryType::Directory { children: ref new_children }) => {

        let old_files = old_children.keys().collect::<BTreeSet<_>>();
        let new_files = new_children.keys().collect::<BTreeSet<_>>();

        let in_both = old_files.intersection(&new_files).map(|s| *s).collect::<BTreeSet<_>>();

        for &deleted_basename in old_files.difference(&in_both) {
          let deleted_node = old_children.get(deleted_basename).unwrap();

          results.push(FileEvent {
            event: ChangeEvent::Delete,
            file: FileResult::make(deleted_node),
          });
        }

        for &created_basename in new_files.difference(&in_both) {
          let created_node = new_children.get(created_basename).unwrap();

          results.push(FileEvent {
            event: ChangeEvent::Create,
            file: FileResult::make(created_node),
          });
        }

        for basename in in_both {
          let before = old_children.get(basename).unwrap();
          let after = old_children.get(basename).unwrap();

          before.diff(after, results);
        }
      }
      (&FsEntryType::File { len: old_length }, &FsEntryType::File { len: new_length }) => {
        match self.mtime.cmp(&new_node.mtime) {
          ::std::cmp::Ordering::Less => {
            results.push(FileEvent {
              event: ChangeEvent::Write,
              file: FileResult::make(&new_node),
            });
          }
          _ => {
            // only generate a notification b/c of length if the mtimes are the same
            if old_length != new_length {
              results.push(FileEvent {
                event: ChangeEvent::Write,
                file: FileResult::make(&new_node),
              });
            }
          }
        }
      }
      (&FsEntryType::Symlink { target: ref old_target, ty: old_ty },
       &FsEntryType::Symlink { target: ref new_target, ty: new_ty }) => {
        if old_target != new_target || old_ty != new_ty {
          results.push(FileEvent {
            event: ChangeEvent::Write,
            file: FileResult::make(&new_node),
          });
        }
      }

      // if the node types differ, just generate create/delete events for everything
      _ => {
        self.gen_events_for_self_and_children(ChangeEvent::Delete, results);
        new_node.gen_events_for_self_and_children(ChangeEvent::Create, results);
      }
    }
  }

  fn gen_events_for_self_and_children(&self, event: ChangeEvent, changes: &mut Vec<FileEvent>) {
    changes.push(FileEvent {
      event: event,
      file: FileResult::make(self),
    });

    match &self.entry {
      &FsEntryType::RootRoot { ref children } => {
        for node in children.values() {
          node.gen_events_for_self_and_children(event, changes);
        }
      }
      _ => (),
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

  fn try_from_node_source<S: NodeSource>(entry: &S) -> Result<Self> {

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

        let sym_target_type = match sym_path.metadata() {
          Ok(sym_meta) => {
            if sym_meta.file_type().is_file() {
              FsItemType::File
            } else if sym_meta.file_type().is_dir() {
              FsItemType::Directory
            } else if sym_meta.file_type().is_symlink() {
              FsItemType::SymlinkUgh
            } else {
              FsItemType::Other
            }
          }
          Err(_) => FsItemType::Other,
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
      path: entry.path().into_owned(),
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

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum FsItemType {
  #[serde(rename="file")]
  File,
  #[serde(rename="dir")]
  Directory,
  #[serde(rename="symlink")]
  SymlinkUgh,
  #[serde(rename="other")]
  Other,
}

struct MetadataFromFs {
  ty: FsItemType,
  mtime: DateTime<Local>,
  len: u64,
}

trait NodeSource {
  fn path(&self) -> Cow<Path>;
  fn metadata(&self) -> Result<MetadataFromFs>;
}

impl<'a> NodeSource for &'a PathBuf {
  fn path(&self) -> Cow<Path> {
    Cow::Borrowed(self.as_ref())
  }

  fn metadata(&self) -> Result<MetadataFromFs> {
    let md = metadata(self).chain_err(|| "unable to read path metadata")?;
    let mtime = system_time_to_date_time(md.modified().chain_err(|| "unable to read mtime")?);

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

impl NodeSource for walkdir::DirEntry {
  fn path(&self) -> Cow<Path> {
    Cow::Borrowed(&self.path())
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
  fn path(&self) -> Cow<Path> {
    Cow::Owned(self.path())
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
    let res = root_node.base.mirror_to_disk();
    match res {
      Ok(_) => (),
      Err(why) => {
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

    // construct a fsrootnode, see if we round-tripped correctly
    let mut second_root_node = FsRootNode::new();
    second_root_node.add_root(tmp.path()).expect("unable to construct pair fs view");

    root_node.base.assert_eq_with_mtime_epsilon(&second_root_node.base, acceptable_epsilon)
  }

  #[test]
  fn single_file_tmp_dir() {
    let tmp = TempDir::new("single").expect("unable to create temp directory");
    let tmp_path = tmp.path().join("test-file");
    let mut tmp_file = File::create(&tmp_path).expect("unable to create test file");

    let mut traversed = 0;
    for entry in WalkDir::new(&tmp_path) {
      let entry = entry.expect("walking directory");
      let node = FsNode::try_from_node_source(&entry).expect("creating node");
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
      let node = FsNode::try_from_node_source(&entry).expect("creating node");
      assert_eq!(node.basename, "test-file");
      assert_eq!(node.path, tmp_path);
      assert_eq!(node.entry, FsEntryType::File { len: 7, });
      traversed += 1;
    }
    assert_eq!(traversed, 1);
  }
}
