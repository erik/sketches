<template>
  <div id="room-container">
    <div class="loading" v-if="loading">
      <i class="fa fa-spinner fa-pulse fa-2x fa-fw"></i>
    </div>

    <div v-if="error" class="error">
      {{ error.toString() }}
    </div>

    <div v-if="meta" class="meta">
      <h2>{{ meta.name }}</h2>
      <span>started by {{ meta.creator }} {{ fromNow(meta.created_at) }}</span>
      <p>{{ meta.description }}</p>
    </div>

    <div id="question-container">
        <div id="ask" class="question">
            <div class="column">
                <span><input v-model="content" placeholder="Ask a question..."></span>
                <span><label>Be anonymous.</label>
                    <input v-model="anon" type="checkbox">
                    </span>
            </div>
            <button v-on:click="ask()">Ask</button>
        </div>

        <div class="question" v-for="q in sortedQuestions">
            <div class="question-title">
                <span class="question-votes" v-on:click="vote(q)">
                    <i v-if="!q.already_voted" class="fa fa-thumbs-up up"></i>
                    <i v-if="q.already_voted" class="fa fa-thumbs-up down"></i>
                    {{ q.votes }}
                </span>

                &ndash;

                <b>{{ q.content }}</b>
            </div>

            <span class="question-meta">
                asked by {{ q.name}}
                <br />
                {{ fromNow(q.timestamp) }}
            </span>
        </div>
    </div>
  </div>
</template>

<style>

#question-container {
  margin-top: 8px;
  display: flex;
  flex-wrap: wrap;
}

.row {
  width: 100%;
}

#ask * {
  float: left;
  margin: 0;
}

#ask .column {
  height: 64px;
  width: 75%;
}

#ask .column span {
  width: 100%;
  height: 32px;
}

#ask .column input {
  max-width: 80%;
  border: 1px solid black;
  padding: 8px;
  text-size: 16px;
}

#ask button {
  width: 25%;
  height: 64px;
  float: left;
  border: 1px solid black;
  border-radius: 4px;
}

@media (max-width: 500px) {
  .question {
    width: 100%;
    margin: 0 0 8px 0;
  }
}

@media (min-width: 500px) {
  .question {
    width: 240px;
    margin: 8px;
  }
}

.question {
  min-height: 60px;
  padding: 8px;
  border: 1px solid #D7DBDD;
  border-radius: 4px;
}

.question-meta {
  font-size: smaller;
}

.up { color: black; }
.down { color: #42b983; }

p {
  margin-bottom: 0px;
}

h2 {
  margin-bottom: 0px;
}

</style>

<script src="./Room.js"></script>
