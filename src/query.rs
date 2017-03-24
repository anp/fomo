use std::ascii::AsciiExt;
use std::borrow::Cow;
use std::default::Default;
use std::path::PathBuf;
use std::usize;

use chrono::{DateTime, Local};
use glob::Pattern;

use fs_view::{FsEntryType, FsItemType, FsNode, FsRootNode};
use glob_wrapper::Globber;
use regex_wrapper::RegexMatcher;

/// A single filesystem query. Heavily inspired by watchman's query DSL.
///
/// As the file system iterator produces potential file matches, they are passed to
/// `expression` for evaluation. Because the expression can
/// be nested many layers deep, this can be used for somewhat complex
/// evaluation of files for inclusion in the query results.
#[derive(Deserialize)]
pub struct Query {
  /// A client-provided identifier for this query.
  id: String,
  expr: QueryExpression,
}

#[derive(Serialize)]
pub struct QueryResult {
  id: String,
  files: Vec<FileResult>,
}

#[derive(Serialize)]
pub struct FileResult {
  path: PathBuf,
  name: String,
  mtime: DateTime<Local>,
  len: u64,
  is_dir: bool,
  link_target: Option<PathBuf>,
  link_target_ty: Option<FsItemType>,
}

impl FileResult {
  fn from(node: &FsNode) -> Self {
    let (len, is_dir, link_target, link_target_type) = match &node.entry {
      &FsEntryType::File { len: l } => (l, false, None, None),
      &FsEntryType::Symlink { ref target, ty } => (0, false, Some(target.clone()), Some(ty)),
      &FsEntryType::Directory { ref children } |
      &FsEntryType::RootRoot { ref children } => (children.len() as u64, true, None, None),
    };

    FileResult {
      path: node.path.clone(),
      name: node.basename.clone(),
      mtime: node.mtime.clone(),
      len: len,
      is_dir: is_dir,
      link_target: link_target,
      link_target_ty: link_target_type,
    }
  }
}

impl Query {
  pub fn eval(self, fs: &FsRootNode) -> QueryResult {
    // we only have one client right now, so the filesystem view should have very low overhead
    // compared to what watchman does, so i don't think we need generators
    let files = fs.iter()
      .filter(|n| self.expr.matches(n))
      .map(|n| FileResult::from(n))
      .collect::<Vec<_>>();

    QueryResult {
      id: self.id,
      files: files,
    }
  }
}

#[derive(Deserialize, Serialize)]
pub enum QueryExpression {
  // begin logical query terms
  /// Returns true if all subexpressions are true.
  #[serde(rename="all")]
  AllOf(Vec<Box<QueryExpression>>),

  /// Returns true if at least one subexpression is true.
  #[serde(rename="any")]
  AnyOf(Vec<Box<QueryExpression>>),

  /// Returns true if its subexpression is false.
  #[serde(rename="not")]
  Not(Box<QueryExpression>),

  // begin metadata related query terms
  /// Returns true if the file has 0 bytes.
  #[serde(rename="empty")]
  Empty,

  /// Returns true if the filesystem entry's metadata has been modified since
  /// the given Unix epoch timestamp.
  ///
  /// The optional `field` can be used to specify whether to compare to only a
  /// single metadata field.
  #[serde(rename="since")]
  Since { time: DateTime<Local>, },

  /// Returns the result of performing the comparison with the current file
  /// size as the left-hand
  /// side, and the specified bytes value as the the right-hand side.
  #[serde(rename="size")]
  Size { cmp: Comparator, bytes: u64, },

  /// Returns true if the file's extension/suffix matches the provided spec
  /// (case-insensitive).
  #[serde(rename="suffix")]
  SuffixCaseInsensitive(String),

  /// Returns true if the file matches the specified file type.
  #[serde(rename="type")]
  Type(FsItemType),

  // begin path-oriented query terms
  /// Returns true if the file has a parent directory that matches the path
  /// provided.
  ///
  /// Optional depth field controls the number of parents/grandparents to
  /// traverse before reaching
  /// a decision.
  #[serde(rename="parent")]
  HasParentDirectory {
    case_insensitive: bool,
    name: String,
    depth: Option<DepthSpec>,
  },

  /// Returns true if the file name (default) or path matches one of the
  /// provided specs exactly (or case insensitively). Matches against the
  /// basename by default.
  #[serde(rename="match")]
  NameMatch {
    spec: Vec<String>,
    #[serde(rename="name", default)]
    match_type: FilenameMatchType,
    case_insensitive: bool,
  },

  /// Returns true if the file name (default) or path matches the provided glob
  /// pattern.
  #[serde(rename="glob")]
  GlobMatch {
    spec: Globber,
    match_type: FilenameMatchType,
    case_insensitive: bool,
  },

  /// Returns true if the file name (default) or path matches the provided
  /// regex.
  ///
  /// Uses Rust's `regex` crate for matching. Syntax documentation:
  ///
  /// https://doc.rust-lang.org/regex/regex/index.html#syntax
  #[serde(rename="regex")]
  Regex {
    spec: RegexMatcher,
    match_type: FilenameMatchType,
  },

  /// Returns true, always.
  #[serde(skip_serializing, skip_deserializing)]
  NoOp,
}

impl QueryExpression {
  fn matches(&self, node: &FsNode) -> bool {
    match self {
      &QueryExpression::AllOf(ref subexprs) => subexprs.iter().all(|subexpr| subexpr.matches(node)),

      &QueryExpression::AnyOf(ref subexprs) => subexprs.iter().any(|subexpr| subexpr.matches(node)),

      &QueryExpression::Not(ref subexpr) => !subexpr.matches(node),

      &QueryExpression::Empty => {
        match node.entry {
          FsEntryType::Directory { ref children } |
          FsEntryType::RootRoot { ref children } => children.len() == 0,
          FsEntryType::File { len } => len == 0,
          FsEntryType::Symlink { .. } => false,
        }
      }

      &QueryExpression::Since { time } => node.mtime >= time,

      &QueryExpression::Size { cmp, bytes } => {
        match node.entry {
          FsEntryType::File { len } => cmp.eval(len, bytes),
          _ => false,
        }
      }

      &QueryExpression::SuffixCaseInsensitive(ref suffix) => {
        let mut halves = node.basename.rsplit('.');
        let found_suffix = halves.next();
        let rest = halves.next();

        if let (Some(_), Some(real_found_suffix)) = (rest, found_suffix) {
          real_found_suffix == suffix
        } else {
          false
        }
      }

      &QueryExpression::Type(ty) => {
        match (ty, &node.entry) {
          (FsItemType::Directory, &FsEntryType::Directory { .. }) => true,
          (FsItemType::Directory, &FsEntryType::RootRoot { .. }) => true,
          (FsItemType::File, &FsEntryType::File { .. }) => true,
          (FsItemType::SymlinkUgh, &FsEntryType::Symlink { .. }) => true,
          _ => false,
        }
      }

      &QueryExpression::HasParentDirectory { case_insensitive, ref name, depth } => {
        // we reverse the path components, so this is essentially walking "up"
        // the tree. we have to skip at least one component from the path,
        // since the first component in a reverse traversal is the basename
        // since we want to supply bounds in the traversal depth, we'll
        //
        // 1. skip the appropriate number of components (1 if there's an upper bound, more if
        //    there's a lower bound supplied)
        // 2. limit the number of path components we'll examine (N if there's an upper bound, and
        //    effectively infinite if there's no upper bound)
        let (skip_amt, take_amt): (usize, usize) = if let Some(depth) = depth {
          let steps = depth.steps as usize;
          match depth.cmp {
            Comparator::Less => (1, steps - 1),
            Comparator::LessEqual => (1, steps),
            Comparator::Equal => (steps - 1, 1),
            Comparator::Greater => (steps, usize::MAX),
            Comparator::GreaterEqual => (steps - 1, usize::MAX),
          }
        } else {
          (1, usize::MAX)
        };

        node.path
          .components()
          .rev()
          .skip(skip_amt)
          .take(take_amt)
          .map(|c| c.as_os_str().to_string_lossy())
          .any(|p| if case_insensitive {
            p.eq_ignore_ascii_case(&name)
          } else {
            &*p == name
          })
      }

      &QueryExpression::NameMatch { ref spec, match_type, case_insensitive } => {
        spec.iter().any(|s| {
          let to_match = node_name_to_match(node, match_type);
          if case_insensitive {
            s.eq_ignore_ascii_case(&*to_match)
          } else {
            s == &*to_match
          }
        })
      }

      &QueryExpression::GlobMatch { ref spec, match_type, case_insensitive } => {
        let spec = if case_insensitive {
          Cow::Owned(Globber(Pattern::new(&spec.as_str().to_lowercase())
            .expect("Unable to compile lowercased glob, even though the case-sensitive version \
                     compiled.")))
        } else {
          Cow::Borrowed(spec)
        };

        let name = node_name_to_match(node, match_type);
        let name = if case_insensitive {
          Cow::Owned(name.to_lowercase())
        } else {
          name
        };

        spec.matches(&name)
      }

      &QueryExpression::Regex { ref spec, match_type } => {
        let name = node_name_to_match(node, match_type);

        spec.is_match(&name)
      }

      &QueryExpression::NoOp => true,
    }
  }
}

impl Default for QueryExpression {
  fn default() -> Self {
    QueryExpression::NoOp
  }
}

fn node_name_to_match(node: &FsNode, match_ty: FilenameMatchType) -> Cow<str> {
  match match_ty {
    FilenameMatchType::Basename => Cow::from(&*node.basename),
    FilenameMatchType::Wholename => node.path.to_string_lossy(),
  }
}

#[derive(Clone, Copy, Deserialize, Serialize)]
pub struct DepthSpec {
  steps: u32,
  cmp: Comparator,
}

#[derive(Clone, Copy, Deserialize, Serialize)]
pub enum Comparator {
  #[serde(rename="lt")]
  Less,
  #[serde(rename="le")]
  LessEqual,
  #[serde(rename="eq")]
  Equal,
  #[serde(rename="ge")]
  GreaterEqual,
  #[serde(rename="gt")]
  Greater,
}

impl Comparator {
  pub fn eval(self, lhs: u64, rhs: u64) -> bool {
    match self {
      Comparator::Less => lhs < rhs,
      Comparator::LessEqual => lhs <= rhs,
      Comparator::Equal => lhs == rhs,
      Comparator::Greater => lhs > rhs,
      Comparator::GreaterEqual => lhs >= rhs,
    }
  }
}

#[derive(Clone, Copy, Deserialize, Serialize)]
pub enum FilenameMatchType {
  #[serde(rename="basename")]
  Basename,
  #[serde(rename="whole")]
  Wholename,
}

impl Default for FilenameMatchType {
  fn default() -> Self {
    FilenameMatchType::Basename
  }
}
