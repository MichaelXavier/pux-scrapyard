var express = require('express');
var app = express();
var bodyParser = require('body-parser');

app.use(express.static('static'));
app.use(express.static('output'));
app.use(bodyParser.json());

var items = [
  {
    text: "sample",
    id: 0
  }
];
var uid = 1;

app.get('/now.json', function(req, res) {
  res.json({now: new Date()});
});

app.get('/hi', function(req, res) {
  res.send("hi");
});

app.get('/items.json', function(req, res) {
  res.json(items);
});

function parseID(s) {
  return parseInt(s, 10);
}

app.post('/items.json', function(req, res) {
  var item = req.body;
  item.id = uid;
  uid++;
  items.push(item);
  res.location("/items/" + item.id + ".json");
  res.status(201).end();
});

app.get('/items/:id.json', function(req, res) {
  var id = parseID(req.params.id);
  var item = items.find(function(x) { return x.id == id;});
  if (item) {
    res.json(item);
  } else {
    res.status(404).end();
  }
});


app.put('/items/:id.json', function(req, res) {
  var id = parseID(req.params.id);
  var item = items.find(function(x) { return x.id == id;});
  if (item) {
    item.text = req.body.text;
    res.end();
  } else {
    res.status(404).end();
  }
});


app.delete('/items/:id.json', function(req, res) {
  var id = parseID(req.params.id);
  var idx = items.findIndex(function(x) { return x.id == id;});
  if (idx >= 0) {
    items.splice(idx, 1);
    res.end();
  } else {
    res.status(404).end();
  }
});

app.listen(8000, function() {
  console.log("Listening on port 8000");
});
