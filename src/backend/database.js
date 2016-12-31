import knexConn from 'knex'

let knex = knexConn({
  client: 'sqlite3',
  connection: { filename: '/tmp/database.sqlite' },
  debug: true
})


export default {
  createSchema () {
    knex.schema.createTableIfNotExists('users', function (table) {
      table.increments()
      table.timestamp(true).defaultTo(knex.fn.now())
      table.string('name')
      table.string('google_id')
      table.string('google_token')
    }).return()


    knex.schema.createTableIfNotExists('rooms', function (table) {
      table.increments()
      table.timestamp(true).defaultTo(knex.fn.now())
      table.string('name')
      table.text('description')
      table.integer('user_id')
      table.boolean('is_archived')
      table.boolean('is_private')

      table.foreign('user_id').references('Users.id')
    }).return()

    knex.schema.createTableIfNotExists('questions', function (table) {
      table.increments()
      table.timestamp(true).defaultTo(knex.fn.now())
      table.integer('user_id')
      table.integer('room_id')
      table.boolean('is_anonymous')
      table.text('content')

      table.foreign('user_id').references('Users.id')
      table.foreign('room_id').references('Rooms.id')
    }).return()

    knex.schema.createTableIfNotExists('votes', function (table) {
      table.increments()
      table.timestamp(true).defaultTo(knex.fn.now())

      table.integer('user_id')
      table.integer('room_id')
      table.integer('question_id')

      table.foreign('user_id').references('Users.id')
      table.foreign('room_id').references('Rooms.id')
    }).return()

    //this.createUser({google_id: 'asdf', google_token: 'asdf', name: 'Seeded User'})
    //this.createRoom({name: 'seeded', description: 'this is a seeded room', user_id: 1})
    //this.createQuestion({user_id: 1, room_id: 1, content: 'this is a seeded question', is_anonymous: false})
    //this.createQuestion({user_id: 1, room_id: 1, content: 'this is an anonymous seeded question', is_anonymous: true})
  },

  createUser ({google_id, google_token, name}) {
    return knex('users')
      .insert({
        name,
        google_id,
        google_token
      })
      .returning('*')
      .then(ids => ({
        name, id: ids[0], google_id, google_token
      }))
  },

  findUser ({id, google_id}) {
    let query = knex('users')
      .select('*')

    if (id)
      return query.where('id', '=', id)

    return query.where('google_id', '=', google_id)
  },

  createRoom ({name, description, user_id}) {
    return knex('rooms')
      .insert({name, description, user_id})
      .returning('*')
      .then(ids => ids[0])
  },

  findRoom ({room_id, name}) {
    return knex('rooms')
      .select([
        'users.name as user_name',
        'rooms.id as id',
        'rooms.name as name',
        'description',
        'is_archived'
      ])
      .innerJoin('users', 'users.id', 'user_id')
      .where('rooms.id', '=', room_id)
  },

  // TODO: archiving, public / private, etc.
  findRooms () {
    return knex('rooms').select('*')
  },

  createQuestion ({user_id, room_id, content, is_anonymous}) {
    return knex('questions')
      .insert({user_id, room_id, content, is_anonymous})
      .returning('*')
      .then(ids => ids[0])
  },

  findQuestions ({room_id, user_id}) {
    return knex
      .select([
        'users.name as name',
        'is_anonymous',
        'q.id as question_id',
        'content'
      ])
      .sumDistinct('all_votes.user_id as votes')
      .sumDistinct('my_votes.user_id as already_voted')
      .from('questions as q')
      .leftJoin('users', 'users.id', 'q.user_id')
      .leftOuterJoin('votes as all_votes', 'q.id', 'all_votes.question_id')
      .leftOuterJoin('votes as my_votes', 'my_votes.user_id', user_id)
      .groupBy(['users.name', 'is_anonymous', 'q.id', 'content'])
  }
}

export var DATABASE = {
  questions: [
    {id: 1, content: 'is this a question?', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()},
    {id: 2, content: 'what are questions?', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()},
    {id: 3, content: 'things and words and things and questions and things and words', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()},
    {id: 4, content: 'asdf', name: 'anonymous', user_id: '12931283123', room_id: 'venice', timestamp: new Date()}
  ],
  votes: { 1: new Set(), 2: new Set(), 3: new Set(), 4: new Set() },
  rooms: {
    venice: {
      id: 'venice',
      name: 'Venice All Hands Meeting',
      description: 'Questions for the weekly allhands meeting',
      created_at: new Date(),
      creator: 'erik'
    }
  },
  users: {}
}
