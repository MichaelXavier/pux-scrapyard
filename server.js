var express = require('express')
var cors = require('cors');
var app = express()

app.use(express.static('static'));
app.use(express.static('output'));
//app.use(cors);

app.get('/now.json', function (req, res) {
  res.json({now: new Date()});
})

app.get('/hi', function(req, res) {
  res.send("hi");
});

app.listen(8000, function() {
  console.log("Listening on port 8000");
});
