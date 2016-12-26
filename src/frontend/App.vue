<template>
    <div id="app">
        <div id="wrapper">
            <nav>
                <router-link to="/">Home</router-link>
                <router-link to="/topic">Create Topic</router-link>
            </nav>

            <main>
                <router-view></router-view>
            </main>
        </div>
    </div>
</template>

<script>
export default {
  name: 'app',
  data () {
    return {}
  }
}
</script>

<style>
#app {
  font-family: 'Avenir', Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: #2c3e50;
}

nav, main {
    margin: 8px;
}

#wrapper {
    padding: 8px;
    max-width: 960px;
    margin: auto;
}

h1, h2 {
  font-weight: normal;
}

ul {
  list-style-type: none;
  padding: 0;
}

li {
  display: inline-block;
  margin: 0;
}

a {
  color: #42b983;
}
</style>
