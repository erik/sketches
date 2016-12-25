<template>
  <div id="room-container">
    {{ $route.params.id }}

    <div class="loading" v-if="loading">
      Loading...
    </div>

    <div v-if="error" class="error">
      {{ error }}
    </div>

    <div v-if="meta" class="meta">
      <h2>{{ meta.name }}</h2>
      <p>discussion started by {{ meta.creator }}</p>
      <p>{{ meta.description }}</p>
    </div>

    <ul class="questions">
      <li v-for="q in questions">
        <h3>{{ q.content }} <small>with {{ q.votes }} votes</small></h3>
        <button v-on:click="vote(q)">+1</button>

        <p>asked by {{ q.name}}<p>
      </li>
    </ul>

  </div>
</template>

<style></style>

<script src="./Room.js"></script>
