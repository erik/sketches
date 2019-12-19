"use strict";

import React, { createElement as h, useState, useCallback } from 'react';
import ReactDOM from 'react-dom';
import { useDropzone } from 'react-dropzone';

const Dropzone = () => {
  const onDrop = useCallback(files => {
    // TODO: handle these files;
    console.log('Received dropped files', files);
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

const DropScreen = () => h('div', { className: 'text-center'}, [
  h('h1', null, 'Drop a route here to begin'),
  h(Dropzone, null, []),
  h('p', null, 'Supported file types: TCX'),
]);

const ErrorScreen = (details) => h('div', {}, [
  h('h1', { className: 'danger' }, 'Oh no! Something went wrong'),
  h('p', null, details || 'an unexpected error occurred')
]);

const App = () => {
  const [screen, setScreen] = useState('drop');

  switch (screen) {
  case 'drop': return DropScreen();

  case 'options':
  default:
    return ErrorScreen('This is not implemented yet!');
  }
};


ReactDOM.render(
  h(App, {}, null),
  document.getElementById('app')
);
