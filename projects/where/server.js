/* jshint esversion: 6 */

const bodyParser = require('body-parser');
const escapeHtml = require('escape-html');
const express = require('express');
const morgan = require('morgan');
const passport = require('passport');
const passportGoogle = require('passport-google-oauth');
const redis = require('redis').createClient();
const session = require('express-session');

require('dotenv').config();

redis.on('error', (err) => console.error(`redis error: ${err}`));

passport.use(new passportGoogle.OAuth2Strategy({
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_SECRET_ID,
    callbackURL: process.env.BASE_URL + '/who/google/callback'
}, (_1, _2, {emails, id}, done) => {
    if (emails.some(e => e.value === process.env.GOOGLE_EMAIL)) {
        return done(null, id);
    }

    return done('who are you');
}));

passport.serializeUser((user, done) => done(null, user));
passport.deserializeUser((user, done) => done(null, user));

const app = express();

app.use(morgan('tiny'));
app.use(bodyParser.urlencoded({extended: true}));

app.use(session({
    secret: process.env.SESSION_SECRET,
    saveUninitialized: true,
    resave: true
}));

app.use(passport.initialize());
app.use(passport.session());

app.get('/who', passport.authenticate('google', {
    scope: ['email']
}));

app.get('/who/google/callback',
        passport.authenticate('google', { failureRedirect: '/who' }),
        (req, res) => {
            req.session.loggedIn = true;
            res.redirect('/here');
        });

app.get('/', (req, res) => { res.sendFile(__dirname + '/where.html'); });

app.get('/where', (req, res) => {
    redis.lrange('where', 0, -1, (err, data) => {
        if (err !== null) {
            console.error('lrange failed', err);
            return res.sendStatus(500);
        }

        return res.send({
            where: data.map(d => JSON.parse(d))
        });
    });
});

app.get('/here', (req, res) => {
    if (!req.session.loggedIn) return res.redirect('/who');
    return res.sendFile(__dirname + '/here.html');
});

app.post('/here', (req, res) => {
    if (!req.session.loggedIn) return res.sendStatus(403);

    const point = JSON.stringify({
        lat: req.body.lat,
        lng: req.body.lng,
        comment: escapeHtml(req.body.comment),
        ts: new Date()
    });

    return redis.lpush('where', point, (err) => {
        if (err !== null) {
            console.error('lpush failed', err);
            res.sendStatus(500);
        } else {
            res.redirect('/');
        }
    });
});

app.listen(process.env.PORT);
