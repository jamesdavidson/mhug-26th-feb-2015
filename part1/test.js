/* A little test runner for the TCP daemon.
 * Requires the Async module by Caolan McMahon.
 */
var net = require('net'), util = require('util'), async = require('async');
WORDS = require('./words.json')
PORT_NUMBER = 1234;
DEBUG = process.env['DEBUG'] == 'true'
CONCURRENCY = Number(process.env['CONCURRENCY']) || 1

for (var i=0; i<CONCURRENCY; i++) {
  async.series([
    pingPong.bind(null,'PING','PONG'),
    setGet.bind(null,WORDS[i],WORDS[i*1]),
    setGet.bind(null,WORDS[i],WORDS[i*2]),
    setGet.bind(null,WORDS[i],WORDS[i*3]),
    setGet.bind(null,WORDS[i],WORDS[i*4]),
    setGet.bind(null,WORDS[i],WORDS[i*5]),
    pingPong.bind(null,'STORE "onlyeone"','INVALID'),
    pingPong.bind(null,'RETRIEVE "too many" 0 "args"','INVALID'),
  ], function (err,data) {
    if (err)
      throw err;
    if (!data.reduce(function(a,b) { return a && b; },true))
      throw 'not all results were true?!';
  });
}

function pingPong (input,expectedOutput,cb) {
  var client = net.connect({port: PORT_NUMBER});
  var buffer = [];
  client.on('connect', function(data) {
    client.write(input + '\r\n');
  });
  client.on('data', function(data) {
    buffer.push(data);
  });
  client.on('end', function() {
    var data = buffer.join('');
    if (data == expectedOutput + '\r\n') {
      if (DEBUG) process.stdout.write(util.format("%s -> %s",input,data));
      cb(null,true);
    }
    else {
      cb('unexpected result! ' + data);
    }
  });
}

function setGet (key,value,finalcb) {
  async.waterfall([
    function(cb){
      var client = net.connect({port: PORT_NUMBER});
      var cmd = 'STORE'+' "'+key+'" '+'"'+value+'"';
      var buffer = [];
      client.on('connect', function(data) {
        client.write(cmd+'\r\n');
      });
      client.on('data', function(data) {
        buffer.push(data);
      });
      client.on('end', function() {
        var data = buffer.join('');
        transaction = Number(data.substr(3));
        if (DEBUG) process.stdout.write(util.format("%s -> %s",cmd,data));
        cb(null,transaction);
      });
    },
    function(tx,cb){
      var client = net.connect({port: PORT_NUMBER});
      var cmd = 'RETRIEVE'+' "'+key+'" '+tx;
      var buffer = [];
      client.on('connect', function(data) {
        client.write(cmd+'\r\n');
      });
      client.on('data', function(data) {
        buffer.push(data);
      });
      client.on('end', function() {
        var data = buffer.join('');
        if (DEBUG) process.stdout.write(util.format("%s -> %s",cmd,data));
        cb(null,data);
      });
    }
  ], function (err, result) {
    if (err) throw err;
    else if (result == value + '\r\n') finalcb(null,true);
    else throw 'unexpected result?!';
  });
}
