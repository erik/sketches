import {Router} from 'express'
import randomstring from 'randomstring'

import {DATABASE} from './database'
import db from './database'


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
  if (!['name', 'description'].every(k => k in req.body &&
                                     req.body[k].length > 0))
    return res.sendStatus(400)

  db.createRoom({
    name: req.body.name,
    description: req.body.description,
    user_id: req.user.id
  }).then(room_id => {
    res.json({room_id})
  })
})


router.get('/api/room', authenticated, (req, res) => {
  db.findRooms()
    .then(rows => {
      res.json({rooms: rows})
    })
})


router.get('/api/room/:id', authenticated, (req, res) => {
  if (!req.params.id)
    return res.sendStatus(404)

  db.findQuestions({room_id: req.params.id, user_id: req.user.id}).then(rows => console.log('ballls', rows))

  Promise.all([
    db.findRoom({room_id: req.params.id}),
    db.findQuestions({room_id: req.params.id, user_id: req.user.id})
  ]).then(values => {
    let rooms = values[0]
    let questions = values[1]

    if (rooms.length === 0) return res.sendStatus(404)

    console.log('rooms->', rooms)
    console.log('questions ->', questions)

    res.json({
      meta: rooms[0],
      questions: questions.map(q => ({
        name: q.is_anonymous ? null : q.name,
        content: q.content,
        votes: q.votes || 0,
        has_voted: q.has_voted > 0,
        created_at: q.created_at
      }))
    })
  })
})


router.post('/api/question/new', authenticated, (req, res) => {
  if (!['room_id', 'content'].every(k => k in req.body &&
                                    req.body[k].length > 0))
    return res.sendStatus(400)

  let question = {
    user_id: req.user.id,
    room_id: req.body.room_id,
    content: req.body.content,
    is_anonymous: req.body.anonymous,
    votes: 1,
    already_voted: true,
    timestamp: new Date()
  }

  db.findRoom({room_id: req.body.room_id})
    .then(rows => {
      if (rows.length === 0) return res.sendStatus(404)

      db.createQuestion(question)
        .then(ids => {
          // TODO: vote for own question
          res.json(Object.assign({id: ids[0]}, question))
        })
    })
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
