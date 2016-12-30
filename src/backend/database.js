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
      name: 'Venice All Hands Meeting',
      description: 'Questions for the weekly allhands meeting',
      created_at: new Date(),
      creator: 'erik'
    }
  },
  users: {}
}
