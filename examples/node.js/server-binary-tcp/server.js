net = require('net');
fs = require('fs');

net.createServer(function(socket){
  var buffer = new Buffer(0, 'binary');

  socket.on("data", function(data){
    buffer = Buffer.concat([buffer, new Buffer(data,'binary')]);
  });

  socket.on("end", function(data) {
    fs.writeFile("image.jpg", buffer, function(err) {
      if(err) {
        console.log(err);
      } else {
        console.log("Socket[" + socket.name + "] closed, wrote data out to sinfo.data");
      }
    }); 

  });

}).listen(5000);

console.log('ready');
