<template>
  <div id="room-container">
    <div class="loading" v-if="loading">
      <i class="fa fa-spinner fa-pulse fa-2x fa-fw"></i>
    </div>

    <div v-if="error" class="error">
      {{ error.toString() }}
    </div>

    <div v-if="meta" id="meta">
      <h2>{{ meta.name }}</h2>
      <span>started by {{ meta.creator }} {{ fromNow(meta.created_at) }}</span>
      <p>{{ meta.description }}</p>
    </div>

    <div v-if="sortedQuestions" id="ask">
        <input id="question-box"
               v-model="content"
               @keyup.enter="ask()"
               placeholder="Ask a question...">

        <input v-model="anon" type="checkbox">
        <label>Ask anonymously</label>
    </div>


    <div id="question-container">
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

#meta {
  text-align: center;
}

#question-container {
  width: 100%;
  margin-top: 32px;
  display: flex;
  flex-wrap: wrap;
}

#ask {
  font-size: 20px;
  padding: 16px;
  width: 100%;
  margin-top: 32px;
  margin-bottom: 32px;
  height: 64px;
}

#question-box {
  height: 32px;
  width: 100%;
  border: 1px solid grey;
  float: left;
  padding: 8px;
  margin: 8px;
  font-size: 16px;
}

#ask * {
  float: right;
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
