import http from 'http'

import express from 'express'
import expressSession from 'express-session'
import cookieParser from 'cookie-parser'
import bodyParser from 'body-parser'

import DATABASE from './database'
import router from './routes'
import auth from './auth'

let app = express()
app.server = http.createServer()

let cookieSecret = 'TODO: move me to config / env'

app.use(cookieParser(cookieSecret))
app.use(bodyParser.json())
app.use(expressSession({
  resave: false,
  saveUninitialized: false,
  httpOnly: false,
  secret: cookieSecret
}))

app.use(express.static('dist/'))

auth.initialize(app)
app.use(router)

app.listen(8080)


export default app
