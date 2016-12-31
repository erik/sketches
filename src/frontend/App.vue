<template>
    <div id="app">
        <div class="container">
            <div class="header">
                <nav class="nav">
                    <div class="nav-left">
                        <router-link class="nav-item" to="/">Home</router-link>
                        <router-link class="nav-item" to="/topic">Create Topic</router-link>
                    </div>
                </nav>
            </div>
        </div>

        <router-view></router-view>
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
</style>
