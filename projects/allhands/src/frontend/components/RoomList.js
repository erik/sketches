export default {
  data () {
    return {
      error: null,
      loading: false,
      rooms: []
    }
  },

  created () { this.fetchData() },

  methods: {
    fetchData () {
      this.error = null
      this.loading = true

      fetch(`/api/room`, {credentials: 'include'})
        .then(r => r.json())
        .then(data => {
          this.loading = false
          this.rooms = data.rooms
        })
        .catch(err => {
          console.log('failed:', err)
          this.error = err
        })
    }
  }
}
