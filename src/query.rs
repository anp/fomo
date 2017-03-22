use std::ascii::AsciiExt;
use std::borrow::Cow;
use std::collections::HashMap;
use std::default::Default;
use std::path::PathBuf;
use std::sync::Mutex;
use std::usize;

use chrono::{DateTime, Local};
use glob::{Pattern, PatternError};
use regex;
use regex::Regex;

use fs_view::{FsEntryType, FsNode, FsRootNode};

lazy_static! {
  static ref GLOBS: Mutex<HashMap<String, Result<Pattern, PatternError>>> = Mutex::new(HashMap::new());
  static ref REGEXES: Mutex<HashMap<String, Result<Regex, regex::Error>>> = Mutex::new(HashMap::new());
}

/// A single filesystem query. Heavily inspired by watchman's query DSL.
///
/// `suffix`, `glob`, and `path` are what watchman called "generators." They
/// are used to build a list of candidate files from the current file system.
///
/// As the generators accumulate potential file matches, they are passed to
/// `expression` for evaluation, if it is provided. Because the expression can
/// be nested many layers deep, this can be used for somewhat complex
/// evaluation of files for inclusion in the query results.
///
/// Finally, if the user specifies deduplication, we will flatten any duplicate
/// file entries before returning the query.
#[derive(Deserialize)]
pub struct Query {
  /// A client-provided identifier for this query.
  id: String,
  #[serde(default)]
  expr: QueryExpression,
  /// Whether to deduplicate results.
  #[serde(default)]
  dedup_results: bool,
}

#[derive(Serialize)]
pub struct QueryResult {
  id: String,
}

impl Query {
  pub fn eval(self, fs: &FsRootNode) -> QueryResult {
    QueryResult { id: self.id, }
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
  Type(NodeType),

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
    spec: String,
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
    spec: String,
    match_type: FilenameMatchType,
    case_insensitive: bool,
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
          (NodeType::Directory, &FsEntryType::Directory { .. }) => true,
          (NodeType::Directory, &FsEntryType::RootRoot { .. }) => true,
          (NodeType::FileRegular, &FsEntryType::File { .. }) => true,
          (NodeType::Symlink, &FsEntryType::Symlink { .. }) => true,
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
          Cow::Owned(spec.to_lowercase())
        } else {
          Cow::Borrowed(spec)
        };

        let name = node_name_to_match(node, match_type);
        let name = if case_insensitive {
          Cow::Owned(name.to_lowercase())
        } else {
          name
        };

        let second_spec = spec.clone();
        let mut globs = GLOBS.lock().unwrap();
        let globber = globs.entry(spec.into_owned()).or_insert_with(|| Pattern::new(&*second_spec));

        match &*globber {
          &Ok(ref globber) => globber.matches(&name),
          &Err(ref why) => panic!("Compiling glob pattern failed: {:?}", why),
        }
      }
      &QueryExpression::Regex { ref spec, match_type, case_insensitive } => {
        let name = node_name_to_match(node, match_type);

        let mut regexes = REGEXES.lock().unwrap();
        let regex = regexes.entry(spec.clone()).or_insert_with(|| Regex::new(&spec));

        match &*regex {
          &Ok(ref r) => r.is_match(&name),
          &Err(ref why) => panic!("Compiling regex pattern failed: {:?}", why),
        }
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

#[derive(Clone, Copy, Deserialize, Serialize)]
pub enum NodeType {
  #[serde(rename="f")]
  FileRegular,
  #[serde(rename="b")]
  FileBlockSpecial,
  #[serde(rename="c")]
  FileCharacterSpecial,
  #[serde(rename="d")]
  Directory,
  #[serde(rename="p")]
  NamedPipeFifo,
  #[serde(rename="l")]
  Symlink,
  #[serde(rename="s")]
  Socket,
}
