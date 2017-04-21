//@flow

const FomoWatcher = require('./index');

const callbacks = {
  onWrite: f => console.log('FILE WRITE: ' + f.path),
  onCreate: f => console.log('FILE CREATE ' + f.path),
  onDelete: f => console.log('FILE DELETE ' + f.path),
  onMetadataChange: f => console.log('FILE METADATA ' + f.path),
  onError: e => console.error('ERROR ' + e.toString()),
  onFatal: e => console.error('FATAL ' + e.toString()),
};

const watcher = new FomoWatcher(callbacks);

watcher.watchDir(
  '/Users/adam/watchman-test',
  (err, files) => {
    if (err) {
      console.error('QUERY ERROR ' + err.toString());
    }

    if (files) {
      for (let f of files) {
        console.log('FOUND FILE ' + f.path);
      }
    }
  },
  {
    suffix: 'js',
  }
);
