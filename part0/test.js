for (i=0;i<5;i++) {
  client = require('net').connect({port:1234});
  client.write('hello\r\n');
  client.pipe(process.stdout);
}
