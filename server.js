import http from 'http';

import express from 'express';
import cookieParser from 'cookie-parser';
import bodyParser from 'body-parser';
import randomstring from 'randomstring';

import socketio from 'socket.io';

let app = express();
app.server = http.createServer();

let io = socketio(app.server);

app.use(cookieParser());
app.use(bodyParser());

app.listen(8080);

// Middleware to assign user with new identity, if valid
// TODO: sign cookies to prevent tampering.
function attachIdentityCookie(req, res, next) {
  var identityCookie = req.cookies.identityCookie;

  // New user, assign a cookie
  if (identityCookie === undefined) {
    identityCookie = randomstring.generate();
    res.cookie('identityCookie', identityCookie);

    console.log('new user, assigining cookie', identityCookie);

  } else {
    console.log('got existing user', identityCookie);
  }

  next();
}


app.get('/', attachIdentityCookie, (req, res) => {
  res.send('homepage');
});


app.post('/api/topic/new', attachIdentityCookie, (req, res) => {
});


app.get('/api/topic/:topic_id', attachIdentityCookie, (req, res) => {
});
