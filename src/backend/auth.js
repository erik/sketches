import passport from 'passport'
import {Strategy as GoogleStrategy} from 'passport-google-oauth20'

import {Router} from 'express'

import {DATABASE} from './database'
import db from './database'


let router = Router()

// load the auth variables
let secrets = require('../../config/secrets.json')


passport.serializeUser((user, done) => done(null, user.id))
passport.deserializeUser((id, done) => db.findUser({id})
                         .then(rows => done(null, rows[0]))
                         .catch(err => done(err, false)))

passport.use(new GoogleStrategy({
  clientID: secrets.google.client_id,
  clientSecret: secrets.google.client_secret,
  callbackURL: secrets.google.callback_url
}, (token, refreshToken, profile, done) => {
  db.findUser({google_id: profile.id})
    .then(rows => {
      if (rows.length === 0)
        return db.createUser({
          google_id: profile.id,
          google_token: token,
          name: profile.displayName
        })
        .then(user => done(null, user))
        .catch(err => done(err, false))

      return done(null, rows[0])
    })
    .catch(err => done(err, false))
}))


export default {
  initialize (app) {
    app.use(passport.initialize())
    app.use(passport.session())

    app.get('/auth/google',
            passport.authenticate('google', {scope : ['profile']}))

    // the callback after google has authenticated the user
    app.get('/auth/google/callback',
            passport.authenticate('google', {
              successRedirect: '/',
              failureRedirect: '/womp'
            }))
  }
}
