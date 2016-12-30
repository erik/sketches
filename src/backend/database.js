import knexConn from 'knex'

let knex = knexConn({
  client: 'sqlite3',
  connection: { filename: '/tmp/database.sqlite' },
  debug: true,
  useNullAsDefault: true
})


export function createSchema() {
  knex.schema.createTableIfNotExists('users', function (table) {
    table.increments()
    table.timestamps()
    table.string('name')
    table.string('google_id')
    table.string('google_token')
  }).return()


  knex.schema.createTableIfNotExists('rooms', function (table) {
    table.increments()
    table.timestamps()
    table.string('name')
    table.text('description')
    table.integer('user_id')

    table.foreign('user_id').references('Users.id')
  }).return()

  knex.schema.createTableIfNotExists('questions', function (table) {
    table.increments()
    table.timestamps()
    table.integer('user_id')
    table.integer('room_id')
    table.boolean('is_anonymous')
    table.text('content')

    table.foreign('user_id').references('Users.id')
    table.foreign('room_id').references('Rooms.id')
  }).return()
}

export function createUser({google_id, google_token, name}) {
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
}

export function findUser({id, google_id}) {
  let query = knex('users')
    .select('*')

  if (id)
    return query.where('id', '=', id)

  return query.where('google_id', '=', google_id)
}

// TODO call this some other way
createSchema()


export default {
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
