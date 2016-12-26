import Vue from 'vue'
import VueRouter from 'vue-router'

import App from './App.vue'
import Room from './components/Room.vue'

Vue.use(VueRouter)

const routes = [
  {path: '/', component: {template: `<div>todo: ask a new question</div>`}},
  {path: '/room/:id', component: Room }
]

const router = new VueRouter({routes})

new Vue({
  el: '#app',
  render: h => h(App),
  router
})
