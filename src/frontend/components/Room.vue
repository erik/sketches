<template>
    <div>
        <div class="loading" v-if="loading">
            <i class="fa fa-spinner fa-pulse fa-2x fa-fw"></i>
        </div>

        <div v-if="error" class="error notification is-error">
            {{ error.toString() }}
        </div>

        <section class="section">
            <div v-if="meta" class="container">
                <h1 class="title">{{ meta.name }}</h1>
                <h2 class="subtitle"> {{ meta.description }}</h2>
                <p>started by {{ meta.user_name }} {{ fromNow(meta.created_at) }}</p>
            </div>

            <div v-if="sortedQuestions" class="container">
                <p class="control is-expanded">
                    <textarea class="textarea"
                              v-model="content"
                              @keyup.enter="ask(true)"
                              placeholder="Ask a question...">
                    </textarea>
                </p>
                <p class="control has-addons has-addons-right">
                    <a class="button is-info" v-on:click="ask(false)">Ask Publically</a>
                    <a class="button" v-on:click="ask(true)">Ask Anonymously</a>
                </p>
            </div>

            <hr />

            <div class="container">
                <div class="box" v-for="q in sortedQuestions">
                    <article class="media">
                        <div class="media-left">
                            <span class="icon is-small has-text-centered"
                                         v-on:click="vote(q)">
                                <i v-if="!q.already_voted" class="fa fa-thumbs-up up"></i>
                                <i v-if="q.already_voted" class="fa fa-thumbs-up down"></i>
                            </span>
                            <br>
                            <span class="has-text-centered">
                                {{ q.votes }}
                            </span>
                        </div>
                        <div class="media-content">
                            <div class="content">
                                <p>
                                    <strong> {{ q.name || 'anonymous' }} </strong>
                                    <small> {{ fromNow(q.created_at) }} </small>
                                    <br>
                                    {{ q.content }}
                                </p>
                            </div>
                        </div>
                    </article>
                </div>
            </div>
        </section>
    </div>
  </template>

<style>
.up { color: black; }
.down { color: #42b983; }
.textarea { resize: none; min-height: 60px !important; }
</style>

<script src="./Room.js"></script>
