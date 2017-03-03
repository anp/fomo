use std::default::Default;
use std::path::PathBuf;

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
#[derive(Deserialize, Serialize)]
pub struct Query {
  /// Filter for files with extensions matching one of the provided suffixes.
  #[serde(default)]
  suffix: Vec<String>,
  /// A list of file globs to match against.
  #[serde(default)]
  glob: Vec<String>,
  /// A list of paths to search, with a depth limit specified.
  #[serde(default)]
  path: Vec<PathQuerySpec>,
  /// A query expression to evaluate against file nodes.
  #[serde(default)]
  expression: QueryExpression,
  /// Whether to deduplicate results.
  #[serde(default)]
  dedup_results: bool,
}

/// A specification of a list of paths to pull files from, with a maximum
/// depth specified to make sure we don't go down any rabbit holes.
#[derive(Deserialize, Serialize)]
pub struct PathQuerySpec {
  /// The path to start searching within.
  #[serde(rename="path")]
  path: String,
  /// The maximum depth to search.
  #[serde(rename="depth", default)]
  depth_limit: TraversalDepth,
}

/// Once the
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
  Since {
    time: u64,
    #[serde(default)]
    field: StatFieldQuery,
  },

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
    name: PathBuf,
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
    include_hidden_files: bool,
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

impl Default for QueryExpression {
  fn default() -> Self {
    QueryExpression::NoOp
  }
}

#[derive(Deserialize, Serialize)]
pub struct DepthSpec {
  steps: u32,
  cmp: Comparator,
}

#[derive(Deserialize, Serialize)]
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

#[derive(Deserialize, Serialize)]
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

#[derive(Deserialize, Serialize)]
pub enum StatFieldQuery {
  #[serde(rename="mtime")]
  ModificationTime,
  #[serde(rename="ctime")]
  CreationTime,
  #[serde(rename="any-change")]
  Both,
}

impl Default for StatFieldQuery {
  fn default() -> Self {
    StatFieldQuery::Both
  }
}

#[derive(Deserialize, Serialize)]
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

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum TraversalDepth {
  #[serde(rename="inf")]
  Infinite,
  #[serde(rename="d")]
  Finite(u32),
}

impl Default for TraversalDepth {
  fn default() -> Self {
    TraversalDepth::Infinite
  }
}
