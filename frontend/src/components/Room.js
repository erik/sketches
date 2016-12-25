export default {
  data () {
    return {
      meta: {
        name: null,
        description: null,
        creator: null
      },

      questions: [],
      room_id: this.$route.params.id,
      loading: false,
      error: null
    }
  },

  created () {
    this.fetchData()
  },

  methods: {
    vote (question) {
      console.log('upvote', question)

      // Optimistically update vote count (might be double vote)
      question.votes += 1

      fetch(`/api/question/${question.id}/vote`, { method: 'post' })
        .then(r => r.json())
        .then(data => { question.votes = data.votes })
        .catch(err => {
          this.error = err
          console.log('failed to upvote', err)
        })
    },

    fetchData () {
      this.error = this.meta = this.questions = null
      this.loading = true

      fetch(`/api/room/${this.room_id}`)
        .then(r => { return r.json(); })
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
