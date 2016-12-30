<template>
  <div>
    <div class="loading" v-if="loading">
      <i class="fa fa-spinner fa-pulse fa-2x fa-fw"></i>
    </div>

    <div v-if="error" class="error">
      {{ error.toString() }}
    </div>

    <ul>
      <li v-for="room in rooms">
        <div>
          <router-link :to="'/room/' + room.id">
            {{room.name}}
          </router-link>

          <p>{{ room.description }}</p>
        </div>
      </li>
    </ul>
  </div>
</template>

<script src="./RoomList.js"></script>
