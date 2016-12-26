// TODO: this is getting hairy, split into multiple files.

import http from 'http'

import express from 'express'
import expressSession from 'express-session'
import cookieParser from 'cookie-parser'
import bodyParser from 'body-parser'
import randomstring from 'randomstring'

import passport from 'passport'
import {Strategy as GoogleStrategy} from 'passport-google-oauth20'

import socketio from 'socket.io'


let app = express()
app.server = http.createServer()

let io = socketio(app.server)

let DATABASE = {
  questions: [
    {id: 1, content: 'is this a question?', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()},
    {id: 2, content: 'what are questions?', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()},
    {id: 3, content: 'things and words and things and questions and things and words', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()},
    {id: 4, content: 'asdf', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()}
  ],
  votes: { 1: new Set(), 2: new Set(), 3: new Set(), 4: new Set() },
  rooms: {
    venice: {
      name: 'Venice All Hands Meeting',
      description: 'Questions for the weekly allhands meeting',
      created_at: new Date(),
      creator: 'erik'
    }
  },
  users: {}
}

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

let cookieSecret = 'TODO: move me to config / env'

app.use(cookieParser(cookieSecret))
app.use(bodyParser.json())
app.use(expressSession({
  resave: false,
  saveUninitialized: false,
  httpOnly: false,
  secret: cookieSecret
}))
app.use(passport.initialize())
app.use(passport.session())
app.use(express.static('dist/'))

app.listen(8080)

app.get('/auth/google', passport.authenticate('google', {scope : ['profile']}))

// the callback after google has authenticated the user
app.get('/auth/google/callback', passport.authenticate('google', {
  successRedirect: '/',
  failureRedirect: '/womp'
}))


function authenticated (req, res, next) {
  if (req.isAuthenticated())
    next()

  else
    res.sendStatus(401)
}


app.get('/', (req, res) => {
  console.log(req.user, req.isAuthenticated())

  if (req.isAuthenticated())
    res.sendFile(__dirname + '/index.html')
  else
    res.send('<a href="/auth/google">Log In with Google</a>')
})


app.post('/api/room/new', authenticated, (req, res) => {
  if (!['name', 'description'].every(k => k in req.body))
    return res.sendStatus(400)

  let room_id = randomstring.generate()

  DATABASE.rooms[room_id] = {
    name: req.body.name,
    description: req.body.description,
    created_at: new Date(),
    user_id: req.user.id
  }

  res.json({room_id: room_id})
})


app.get('/api/room/:id', authenticated, (req, res) => {
  if (!req.params.id || !DATABASE.rooms[req.params.id])
    return res.sendStatus(404)

  res.json({
    meta: DATABASE.rooms[req.params.id],
    questions: DATABASE.questions
      .filter(q => q.room_id == req.params.id)
      .map(q => ({
        id: q.id,
        name: q.name,
        content: q.content,
        timestamp: q.timestamp,
        votes: (DATABASE.votes[q.id] || new Set()).size,
        already_voted: (DATABASE.votes[q.id] || new Set()).has(req.user.id)
      }))
  })
})


app.post('/api/question/new', authenticated, (req, res) => {
  if (!['room_id', 'content'].every(k => k in req.body))
    return res.sendStatus(400)

  if (!DATABASE.rooms[req.body.room_id])
    return res.sendStatus(404)

  let q_id = randomstring.generate(3)

  DATABASE.questions.push({
    id: q_id,
    content: req.body.content,
    room_id: req.body.room_id,
    name:  req.body.anonymous ? null : req.user.name,
    user_id: req.user.id,
    timestamp: new Date()
  })

  DATABASE.votes[q_id] = new Set()

  res.json({})
})


app.post('/api/question/:id/delete', authenticated, (req, res) => {
  let index = DATABASE.questions.findIndex(q => q.id == req.params.id);

  if (index === -1)
    return res.sendStatus(404)

  if (DATABASE.questions[index].user_id !== req.user.id)
    return res.sendStatus(401)

  DATABASE.questions.splice(index, 1)
  delete DATABASE.votes[req.params.id]

  res.json({})
})


app.post('/api/question/:id/vote', authenticated, (req, res) => {
  let votes = DATABASE.votes[req.params.id]

  if (!votes)
    return res.sendStatus(404)

  console.log(req.body)

  if (req.body.down)
    votes.delete(req.user.id)
  else
    votes.add(req.user.id)

  res.json({votes: votes.size})
})


export default app
