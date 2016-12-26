import http from 'http'

import express from 'express'
import cookieParser from 'cookie-parser'
import bodyParser from 'body-parser'
import randomstring from 'randomstring'

import socketio from 'socket.io'


let app = express()
app.server = http.createServer()

let io = socketio(app.server)


app.use(cookieParser())
app.use(bodyParser())
app.use((req, res, next) => { res.io = io; next(); })

app.use(express.static('dist/'))

app.listen(8080)


// Middleware to assign user with new identity, if valid
// TODO: sign cookies to prevent tampering.
function attachIdentityCookie(req, res, next) {
  console.log(req.cookies, req.signedCookies)
  let identityCookie = req.cookies.identityCookie

  // New user, assign a cookie
  if (identityCookie === undefined) {
    identityCookie = randomstring.generate()
    res.cookie('identityCookie', identityCookie)

    console.log('new user, assigining cookie', identityCookie)
  } else {
    console.log('got existing user', identityCookie)
  }

  req.identityCookie = identityCookie
  next()
}


let DATABASE = {
  questions: [{
    id: 1,
    content: 'is this a question?',
    name: 'anonymous',
    user_id: '12931283123',
    room_id: 'venice',
    timestamp: new Date()
  },{
    id: 2,
    content: 'what are questions?',
    name: 'anonymous',
    user_id: '12931283123',
    room_id: 'venice',
    timestamp: new Date()
  },{
    id: 3,
    content: 'things and words and things and questions and things and words',
    name: 'anonymous',
    user_id: '12931283123',
    room_id: 'venice',
    timestamp: new Date()
  },{
    id: 4,
    content: 'asdf',
    name: 'anonymous',
    user_id: '12931283123',
    room_id: 'venice',
    timestamp: new Date()
  },],
  votes: { 1: new Set(), 2: new Set(), 3: new Set(), 4: new Set() },
  rooms: {
    venice: {
      name: 'Venice All Hands Meeting',
      description: 'Questions for the weekly allhands meeting',
      created_at: new Date(),
      creator: 'erik'
    }
  }
}


app.get('/', attachIdentityCookie, (req, res) => {
  res.sendFile(__dirname + '/index.html')
})


app.post('/api/room/new', attachIdentityCookie, (req, res) => {
  if (!['name', 'description'].every(k => k in req.body))
    return res.sendStatus(400)

  let room_id = randomstring.generate()

  DATABASE.rooms[room_id] = {
    name: req.body.name,
    description: req.body.description,
    created_at: new Date(),
    creator: req.identityCookie
  }

  res.json({room_id: room_id})
})


app.get('/api/room/:id', attachIdentityCookie, (req, res) => {
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
        already_voted: (DATABASE.votes[q.id] || new Set()).has(req.identityCookie)
      }))
  })
})


app.post('/api/question/new', attachIdentityCookie, (req, res) => {
  if (!['room_id', 'content'].every(k => k in req.body))
    return res.sendStatus(400)

  if (!DATABASE.rooms[req.body.room_id])
    return res.sendStatus(404)

  let q_id = randomstring.generate(3)

  DATABASE.questions.push({
    id: q_id,
    content: req.body.content,
    room_id: req.body.room_id,
    name:  req.body.anonymous ? null : req.body.name,
    user_id: req.identityCookie,
    timestamp: new Date()
  })

  DATABASE.votes[q_id] = new Set()

  res.json(DATABASE)
})


app.post('/api/question/:id/delete', attachIdentityCookie, (req, res) => {
  let index = DATABASE.questions.findIndex(q => q.id == req.params.id);

  if (index === -1)
    return res.sendStatus(404)

  if (DATABASE.questions[index].user_id !== req.identityCookie)
    return res.sendStatus(401)

  DATABASE.questions.splice(index, 1)
  delete DATABASE.votes[req.params.id]

  res.json({})
})


app.post('/api/question/:id/vote', attachIdentityCookie, (req, res) => {
  let votes = DATABASE.votes[req.params.id]

  if (!votes)
    return res.sendStatus(404)

  console.log(req.body)

  if (req.body.down)
    votes.delete(req.identityCookie)
  else
    votes.add(req.identityCookie)

  res.json({votes: votes.size})
})


export default app
