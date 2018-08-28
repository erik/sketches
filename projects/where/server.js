/* jshint esversion: 6 */

const bodyParser = require('body-parser');
const escapeHtml = require('escape-html');
const express = require('express');
const expressHandlebars = require('express-handlebars');
const handlebars = require('handlebars');
const morgan = require('morgan');
const moment = require('moment');
const passport = require('passport');
const passportGoogle = require('passport-google-oauth');
const redis = require('redis').createClient();
const session = require('express-session');

require('dotenv').config();

redis.on('error', err => console.error(`redis error: ${err}`));

const app = express();

app.use(morgan('common'));
app.use(bodyParser.urlencoded({ extended: true }));
app.engine(
  'html',
  expressHandlebars({
    helpers: {
      humanize: ts => moment(ts).fromNow(),
      json: val => new handlebars.SafeString(JSON.stringify(val)),
      unsafe: val => new handlebars.SafeString(val)
    }
  })
);
app.set('view engine', 'handlebars');

app.use(
  session({
    secret: process.env.SESSION_SECRET,
    saveUninitialized: true,
    resave: false,
    maxAge: 365 * 24 * 60 * 60 * 1000  // ~1 year
  })
);

passport.use(
  new passportGoogle.OAuth2Strategy(
    {
      clientID: process.env.GOOGLE_CLIENT_ID,
      clientSecret: process.env.GOOGLE_SECRET_ID,
      callbackURL: process.env.BASE_URL + '/who/google/callback'
    },
    (_1, _2, { emails, id }, done) => {
      if (emails.some(e => e.value === process.env.GOOGLE_EMAIL)) {
        return done(null, id);
      }

      return done('who are you');
    }
  )
);

passport.serializeUser((user, done) => done(null, user));
passport.deserializeUser((user, done) => done(null, user));

app.use(passport.initialize());
app.use(passport.session());

function requireAuth(req, res, next) {
  if (!req.session.loggedIn) {
    return res.redirect('/who');
  }

  return next();
}

function where() {
  return new Promise((resolve, reject) => {
    redis.hgetall('where', (err, data) => {
      if (err !== null) {
        console.error('hgetall failed', err);
        return reject(err);
      }

      return resolve(
        Object.values(data || {})
          .map(d => JSON.parse(d))
          .sort((a, b) => -a.ts.localeCompare(b.ts))
      );
    });
  });
}

function here(lat, lng, comment) {
  const createdAt = new Date();
  const redisKey = createdAt.toISOString();

  const point = JSON.stringify({
    lat,
    lng,
    comment: escapeHtml(comment),
    ts: createdAt,
    key: redisKey
  });

  return new Promise((resolve, reject) => {
    redis.hset('where', redisKey, point, err => {
      if (err !== null) {
        console.error('hset failed', err);
        return reject(err);
      }

      return resolve(null);
    });
  });
}

app.get('/', (req, res) => {
  where()
    .then(points => {
      res.render('where.html', { who: process.env.WHO, points });
    })
    .catch(() => res.sendStatus(500));
});

const passportAuthenticate = passport.authenticate('google', {
  scope: ['email'],
  failureRedirect: '/who'
});

app.get('/who', passportAuthenticate);

app.get('/who/google/callback', passportAuthenticate, (req, res) => {
  req.session.loggedIn = true;
  res.redirect('/here');
});

app.get('/here', requireAuth, (req, res) => {
  where()
    .then(points => res.render('here.html', { points }))
    .catch(() => res.sendStatus(500));
});

app.post('/here', requireAuth, (req, res) => {
  here(req.body.lat, req.body.lng, req.body.comment)
    .then(() => res.redirect('/'))
    .catch(() => res.sendStatus(500));
});

app.post('/here/:id/delete', requireAuth, (req, res) => {
  if (!req.params.id) return res.sendStatus(400);

  redis.hdel('where', req.params.id, err => {
    if (err !== null) {
      console.error('hdel failed', err);
      return res.sendStatus(500);
    }

    return res.redirect('/here');
  });
});

app.listen(process.env.PORT);
