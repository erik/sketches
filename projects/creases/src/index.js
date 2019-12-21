"use strict";

import React, { createElement as h, useState, useCallback } from 'react';
import ReactDOM from 'react-dom';
import { useDropzone } from 'react-dropzone';

import parser from './parser.js';


const Dropzone = (setScreen) => () => {
  const onDrop = useCallback((files) => {
    // Only take the first file uploaded;
    const file = files[0];

    parser.parseFile(file)
      .then(it => console.log('parsed to:', it))
      .then(() => setScreen(AppScreens.RENDER_PDF))
      .catch(err => {
        console.error(err);
        window.alert("Something went wrong, maybe something's wrong with that file?");
      });
  }, []);

  const { getRootProps, getInputProps, isDragActive } = useDropzone({ onDrop });

  return h('div', { className: 'dropzone', ...getRootProps() }, [
    h('input', getInputProps(), null),
    h('p', null, [
      isDragActive
        ? 'riiiiight here'
        : 'Drag and drop here (or click to select)'
    ])
  ]);
};

const DropScreen = (setScreen) => h('div', { className: 'text-center'}, [
  h('h1', null, 'Drop a route here to begin'),
  h(Dropzone(setScreen), null, []),
  h('p', null, 'Supported file types: FIT'),
]);

const ErrorScreen = (details) => h('div', {}, [
  h('h1', { className: 'danger' }, 'Oh no! Something went wrong'),
  h('p', null, details || 'an unexpected error occurred')
]);

const AppScreens = {
  DROP: 'drop',
  RENDER_OPTIONS: 'render_options',
  RENDER_PDF: 'render_pdf',
  ERROR: 'error'
};

const App = () => {
  const [screen, setScreen] = useState(AppScreens.DROP);

  switch (screen) {
  case AppScreens.DROP: return DropScreen(setScreen);
  case AppScreens.RENDER_OPTIONS:
  case AppScreens.RENDER_PDF:
    return ErrorScreen('This is not implemented yet!');

  default:
    return ErrorScreen(`A bug! Unknown screen type: ${screen}`);
  }
};


ReactDOM.render(
  h(App, {}, null),
  document.getElementById('app')
);
