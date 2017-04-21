// @flow
const child_process = require('child_process');
const events = require('events');
const path = require('path');
const readline = require('readline');
const uuid = require('uuid');

/*::
type QueryExpression =
  "empty" | {
    all: Array<QueryExpression>
  } | {
    any: Array<QueryExpression>
  } | {
    not: QueryExpression
  } | {
    since: Date,
  } | {
    size: {
      bytes: number,
      cmp: Comparator,
    }
  } | {
    suffix: string
  } | {
    type: FileSystemItemType
  } | {
    parent: {
      case_insensitive: boolean,
      name: string,
      depth?: {
        steps: number,
        cmp: Comparator,
      },
    }
  } | {
    match: {
      spec: Array<string>,
      name: FilenameMatchType,
      case_insensitive: boolean,
    },
  } | {
    glob: {
      spec: string,
      match_type: FilenameMatchType,
      case_insenstive: bool,
    },
  } | {
    regex: {
      spec: string,
      match_type: FilenameMatchType,
    },
  };

interface Query {
  id: string;
  root: string;
  expr: QueryExpression;
  watch?: boolean;
}

interface FileResult {
  path: string,
  name: string,
  mtime: Date,
  len: number,
  is_dir: boolean,
  link_target?: string,
  link_target_ty?: FileSystemItemType,
}

interface FileEvent {
  event: ChangeEvent,
  file: FileResult,
}

interface QueryResult {
  id?: string,
  error?: string,
  humanError?: string,
  queryString?: string,
  files?: Array<FileResult>,
  root?: string,
  changes?: Array<FileEvent>,
}

type ChangeEvent = "create" | "delete" | "write" | "metadata";
type Comparator = "lt" | "le" | "eq" | "ge" | "gt";
type FilenameMatchType = "basename" | "whole";
type FileSystemItemType = "file" | "dir" | "symlink" | "other";

type ErrorCallback = (e: Error) => void;
type FileCallback = (file: FileResult) => void;
type QueryCallback = (err?: Error, result?: Array<FileResult>) => void;

interface CallbackRecord {
  callback: QueryCallback,
  original: string,
}

interface WatcherCallbacks {
  onWrite: FileCallback;
  onCreate: FileCallback;
  onDelete: FileCallback;
  onMetadataChange: FileCallback;
  onError: ErrorCallback;
  onFatal: ErrorCallback;
}
*/

function binaryPath() {
  // TODO setup for actual prod and different platforms/targets
  return '/Users/adam/.fomo_target/debug/fomo-bin';
}

class FomoWatcher {
  /*::
    child: child_process$ChildProcess;
    logs: Array<string>;
    queries: Map<string, CallbackRecord>;
    errReadline: readline$Interface;
    outReadline: readline$Interface;

    onWrite: FileCallback;
    onCreate: FileCallback;
    onDelete: FileCallback;
    onMetadataChange: FileCallback;
    onError: ErrorCallback;
    onFatal: ErrorCallback;
  */

  constructor(callbacks /*:WatcherCallbacks*/) {
    this.onWrite = callbacks.onWrite;
    this.onCreate = callbacks.onCreate;
    this.onDelete = callbacks.onDelete;
    this.onMetadataChange = callbacks.onMetadataChange;
    this.onError = callbacks.onError;
    this.onFatal = callbacks.onFatal;

    this.child = child_process.spawn(binaryPath(), {
      stdio: ['pipe', 'pipe', 'pipe'],
    });

    this.logs = [];
    this.queries = new Map();

    this.errReadline = readline.createInterface({
      input: this.child.stderr,
    });
    this.outReadline = readline.createInterface({
      input: this.child.stdout,
    });

    this.errReadline.on('line', line => this._handleStdErr(line));
    this.outReadline.on('line', line => this._handleStdOut(line));

    process.on('exit', () => {
      try {
        this.close();
      } catch (e) {
        // lol
      }
    });
  }

  // prettier-ignore
  watchDir(directory /*: string*/ , callback /*: QueryCallback*/ , expr /*: ?QueryExpression*/ ) {
    let id = uuid();
    let query = {
      id: id,
      root: directory,
      expr: expr,
      watch: true
    };

    let msg = JSON.stringify(query);

    this.queries.set(id, {
      callback,
      original: msg
    });

    this.child.stdin.write(msg + '\n');
  }

  _handleStdErr(line /*: string*/) {
    this.logs.push(line);
  }

  _handleStdOut(line /*: string*/) {
    let msg /*: QueryResult*/;
    try {
      msg = JSON.parse(line);
    } catch (e) {
      // TODO if this happens then something very bad has occurred, probably need to throw or something?
      this.onFatal(e);
      return;
    }

    if (msg.queryString) {
      this._handleMessageError(msg);
    } else if (msg.files) {
      let rec = this.queries.get(msg.id);

      if (rec && this.queries.delete(msg.id)) {
        rec.callback(undefined, msg.files);
      } else {
        this.onFatal(new Error('invariant violation! missing callback'));
      }
    } else if (msg.changes) {
      for (let change of msg.changes) {
        if (change.event && change.file) {
          // let's actually dispatch this
          switch (change.event) {
            case 'create':
              this.onCreate(change.file);
              break;
            case 'delete':
              this.onDelete(change.file);
              break;
            case 'write':
              this.onWrite(change.file);
              break;
            case 'metadata':
              this.onMetadataChange(change.file);
              break;
            default:
              this.onFatal(new Error('lol fallthrough is terrible'));
          }
        } else {
          this.onFatal(new Error('invariant violation! change msg missing critical fields'));
        }
      }
    } else {
      this.onFatal(
        new Error('invariant violation! response missing queryString, files, and changes')
      );
    }
  }

  _handleMessageError(msg /*: any*/) {
    // this means there was an error
    if (msg.files) {
      this.onFatal(new Error('invariant violation! got bad response from native side'));
    }

    // now we need to find the appropriate callback!
    let rec;
    let error = new Error(`${msg.error}: ${msg.humanError}`);
    if (msg.id) {
      rec = this.queries.get(msg.id);
      this.queries.delete(msg.id);
    } else {
      let idToDelete;

      for (let thisId of this.queries.keys()) {
        let newRec = this.queries.get(thisId);
        if (newRec && newRec.original === msg.queryString) {
          rec = newRec;
          idToDelete = thisId;
          break;
        }
      }

      if (idToDelete) {
        this.queries.delete(idToDelete);
      }
    }

    try {
      if (rec) {
        rec.callback(error);
      } else {
        throw new Error('invariant violation! missing callback or exception in callback');
      }
    } catch (e) {
      this.onFatal(e);
    }
  }

  close() {
    this.errReadline.close();
    this.outReadline.close();
    this.child.kill();
  }
}

module.exports = FomoWatcher;
