import {Router} from 'express'
import randomstring from 'randomstring'

import DATABASE from './database'


let router = Router()


function authenticated (req, res, next) {
  if (req.isAuthenticated())
    next()

  else
    res.sendStatus(401)
}


router.get('/', (req, res) => {
  console.log(req.user, req.isAuthenticated())

  if (req.isAuthenticated())
    res.sendFile(__dirname + '/views/index.html')
  else
    res.sendFile(__dirname + '/views/login.html')
})


router.post('/api/room/new', authenticated, (req, res) => {
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


router.get('/api/room/:id', authenticated, (req, res) => {
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


router.post('/api/question/new', authenticated, (req, res) => {
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


router.post('/api/question/:id/delete', authenticated, (req, res) => {
  let index = DATABASE.questions.findIndex(q => q.id == req.params.id);

  if (index === -1)
    return res.sendStatus(404)

  if (DATABASE.questions[index].user_id !== req.user.id)
    return res.sendStatus(401)

  DATABASE.questions.splice(index, 1)
  delete DATABASE.votes[req.params.id]

  res.json({})
})


router.post('/api/question/:id/vote', authenticated, (req, res) => {
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


export default router
