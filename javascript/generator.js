var fs = require('fs');

fs.readFile(__filename, function(err, res) {
  console.log(err, res) ;
});

var readFile2 = function(filename) {
  return function(cb) {
    fs.readFile(filename, cb);
  };
};

var cofy = function(cbFn) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    return function(cb) {
      args.push(cb);
      cbFn.apply(cbFn, args);
    };
  };
};

var readFile = cofy(fs.readFile);
var stat = cofy(fs.stat);
var readDir = cofy(fs.readdir);

// generator
var gen = function* () {
  try {
    var files = yield readDir('./');
    var stats = [];
    for (var i=0; i < files.length; i++) {
      var st = yield stat(files[i]);
      stats.push(st.size);
    }
    console.log(stats) ;
  } catch (e) {
    console.log('exception occoured!') ;
  }
};

// scheduler
var task = function(gen) {
  var co = gen();

  var onNext = function(err, value) {
    if (err) {
      co.throw(err);
    } else {
      var res = co.next(value);
      var fn = res.value;

      if (res.done) {
        console.log('FINISH') ;
      } else {
        fn(onNext);
      }
    }
  };

  onNext();
};

task(gen);

var work = function() {
  fs.readdir('./', function(err, fileList) {
    if (err) {
      console.log(err) ;
      return;
    }

    var stats = [];
    var cnt = fileList.length ;
    var i = 0;

    fileList.forEach(function(file) {
      fs.stat(file, function(err, stat) {
        if (err) {
          console.log(err) ;
          return;
        }

        stats.push(stat.size);
        i++;

        if (i === cnt) {
          console.log(stats) ;
        }
      });
    });
  });
};

work();
