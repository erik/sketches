import Vue from 'vue'
import VueRouter from 'vue-router'

import App from './App.vue'

import Room from './components/Room.vue'
import RoomList from './components/RoomList.vue'


Vue.use(VueRouter)

const routes = [
  {path: '/', component: RoomList },
  {path: '/room/:id', component: Room },
  {path: '*', component: { template: '<h1>Where are you going?</h1>'}}
]

const router = new VueRouter({routes})

new Vue({
  el: '#app',
  render: h => h(App),
  router
})
