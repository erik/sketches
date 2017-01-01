<template>
  <div>
    <div v-if="error" class="error notification is-danger">
      {{ error.toString() }}
    </div>

    <section class="section">
        <h1 class="title">Available rooms</h1>
    </section>

    <section class="section">
        <div class="content">
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
    </section>
  </div>
</template>

<script src="./RoomList.js"></script>
