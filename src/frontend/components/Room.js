import moment from 'moment'


export default {
  data () {
    return {
      meta: {
        name: null,
        description: null,
        creator: null,
        created_at: new Date()
      },

      questions: [],
      room_id: this.$route.params.id,
      loading: false,
      error: null,
      content: null
    }
  },

  created () {
    this.fetchData()
  },

  computed: {
    sortedQuestions () {
      return this.questions.sort((a, b) => {
        if (a.votes !== b.votes)
          return b.votes - a.votes

        return new Date(b.created_at) - new Date(a.created_at)
      })
    }
  },

  methods: {
    fromNow (time) {
      return moment(time).fromNow()
    },

    ask (anonymous) {
      // Don't ask blank questions
      if (this.content.trim() === '')
        return

      fetch('/api/question/new', {
        method: 'post',
        credentials: 'include',
        headers: {'Content-type': 'application/json'},
        body: JSON.stringify({
          content: this.content.trim(),
          room_id: this.room_id,
          anonymous: anonymous
        })
      })
        .then(r => r.json())
        .then(question => {
          this.questions.unshift(question)
        })
        .catch(err => this.error = err)

      this.content = ""
    },

    vote (question) {
      fetch(`/api/question/${question.id}/vote`, {
        method: 'post',
        credentials: 'include',
        headers: {'Content-type': 'application/json'},
        body: JSON.stringify({ down: question.already_voted })
      }).then(r => r.json())
        .then(data => {
          question.votes = data.votes
          question.already_voted = !question.already_voted
        })
        .catch(err => {
          this.error = err
          console.log('failed to upvote', err)
        })
    },

    fetchData () {
      this.error = this.meta = null
      this.questions = []
      this.loading = true

      fetch(`/api/room/${this.room_id}`, {credentials: 'include'})
        .then(r => r.json() )
        .then(data => {
          this.loading = false
          this.meta = data.meta
          this.questions = data.questions
        })
        .catch(err => {
          console.log('Failed', err)
          this.error = err
        })
    }
  }
}
