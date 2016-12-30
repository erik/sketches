import passport from 'passport'
import {Strategy as GoogleStrategy} from 'passport-google-oauth20'

import {Router} from 'express'

import DATABASE from './database'


let router = Router()

// load the auth variables
let secrets = require('../../config/secrets.json')


passport.serializeUser((user, done) => done(null, user.id))
passport.deserializeUser((id, done) => done(null, DATABASE.users[id]))

passport.use(new GoogleStrategy({
  clientID: secrets.google.client_id,
  clientSecret: secrets.google.client_secret,
  callbackURL: secrets.google.callback_url
}, (token, refreshToken, profile, done) => {
  let user = DATABASE.users[profile.id] = {
    id: profile.id,
    token: token,
    name: profile.displayName
  }

  console.log('got my user', user)

  return done(null, user)
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
