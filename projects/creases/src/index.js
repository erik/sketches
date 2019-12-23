"use strict";

import React, { createElement as h, useState, useCallback } from 'react';
import ReactDOM from 'react-dom';
import { useDropzone } from 'react-dropzone';
import * as Leaflet from 'react-leaflet';

import parser from './parser.js';


const Dropzone = (switchScreen) => () => {
  const onDrop = useCallback((files) => {
    // Only take the first file uploaded;
    const file = files[0];

    parser.parseFile(file)
      .then(it => {
        console.log('parsed to:', it);
        switchScreen(AppScreens.RENDER_PDF, {
          // TODO: make this configurable.
          options: {tileKm: 50},
          route: it,
        });
      })
      .catch(err => {
        console.error(err);
        switchScreen(AppScreens.ERROR, `Something went wrong, maybe something's wrong with that file?\n\n ${err}`);
      });
  }, []);

  const { getRootProps, getInputProps, isDragActive } = useDropzone({ onDrop });

  return h('div', { className: 'dropzone', ...getRootProps() }, [
    h('h1', null, 'Drop a route here to begin'),
    h('input', getInputProps(), null),
    h('p', null, [
      isDragActive
        ? 'riiiiight here'
        : '(or click to select)'
    ])
  ]);
};

const DropScreen = (setScreen) => h('div', { className: 'text-center'}, [
  h(Dropzone(setScreen), null, []),
  h('p', null, 'Supported file types: TCX'),
]);

const ErrorScreen = (details) => h('div', {}, [
  h('h1', { className: 'danger' }, 'Oh no! Something went wrong'),
  h('p', null, details || 'an unexpected error occurred'),
  h('p', null, 'Reload the page to start fresh.')
]);

const RenderRouteScreen = (switchScreens, data) => {
  console.log('your route is this', data);
  return 'wow, how cool.';
};

const RenderOptionsScreen = (switchScreens, data) => {
  return h(Leaflet.Map, {center: [51.50, 0], zoom: 13}, []);
};

const AppScreens = {
  DROP: 'drop',
  RENDER_OPTIONS: 'render_options',
  RENDER_PDF: 'render_pdf',
  ERROR: 'error'
};

const App = () => {
  const [screen, setScreen] = useState({
    state: AppScreens.DROP,
    data: null
  });

  const switchScreen = (state, data) => setScreen({state, data});

  switch (screen.state) {
  case AppScreens.DROP:
    return DropScreen(switchScreen);

  case AppScreens.RENDER_PDF:
    return RenderRouteScreen(switchScreen, screen.data);

  case AppScreens.RENDER_OPTIONS:
    switchScreen(AppScreens.ERROR, 'This is not yet implemented');
    return '';

  case AppScreens.ERROR:
    return ErrorScreen(screen.data || 'A very undefined bug');

  default:
    return ErrorScreen(`A bug! Unknown screen type: ${screen}`);
  }
};


ReactDOM.render(
  h(App, {}, null),
  document.getElementById('app')
);
