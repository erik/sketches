import { h, app } from 'hyperapp';


const state = [];

const actions = {};

const view = (state, actions) => (
    h('h1', {}, ['WOW'])
);

app(state, actions, view, document.body);
