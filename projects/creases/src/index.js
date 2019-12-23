"use strict";

import React, { createElement as h, useState, useCallback, useEffect } from 'react';
import ReactDOM from 'react-dom';
import { useDropzone } from 'react-dropzone';
import * as Leaflet from 'leaflet';

import parser from './parser.js';


const Dropzone = ({switchScreen}) => {
  const onDrop = useCallback((files) => {
    // Only take the first file uploaded;
    const file = files[0];

    parser.parseFile(file)
      .then(it => {
        console.log('parsed to:', it);
        switchScreen(AppScreens.RENDER_OPTIONS, {route: it});
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

const DropScreen = ({switchScreen}) => h('div', { className: 'text-center'}, [
  h(Dropzone, {switchScreen}, []),
  h('p', null, 'Supported file types: GPX, TCX'),
]);

const ErrorScreen = ({details}) => h('div', {}, [
  h('h1', { className: 'danger' }, 'Oh no! Something went wrong'),
  h('p', null, details || 'an unexpected error occurred'),
  h('p', null, 'Reload the page to start fresh.')
]);

const RenderRouteScreen = ({switchScreens, data}) => {
  console.log('your route is this', data);
  return 'wow, how cool.';
};

const RenderOptionsScreen = ({switchScreens, data}) => {
  const mapRef = React.useRef(null);

  useEffect(() => {
    const points = data.route.routePoints.map(it => [it.latitude, it.longitude]);
    const line = new Leaflet.polyline(points);

    const waypoints = data.route.wayPoints.map(it => new Leaflet.marker(
      [it.latitude, it.longitude], {title: it.name}));

    mapRef.current = new Leaflet.map('map', {
      center: [0, 0],
      zoom: 13,
      layers: [
        ...waypoints,
        line,
        new Leaflet.TileLayer(
          "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", {
            attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
          }),
      ]
    });

    mapRef.current.fitBounds(line.getBounds());
  }, []);

  return h('div', {id: 'map', style: {height: '800px'}}, null);
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
    return h(DropScreen, {switchScreen});

  case AppScreens.RENDER_PDF:
    return h(RenderRouteScreen, {switchScreen, data: screen.data});

  case AppScreens.RENDER_OPTIONS:
    return h(RenderOptionsScreen, {switchScreen, data: screen.data});

  case AppScreens.ERROR:
    return h(ErrorScreen, {details: screen.data || 'A very undefined bug'});

  default:
    return h(ErrorScreen, {details: `A bug! Unknown screen type: ${screen}`});
  }
};


ReactDOM.render(
  h(App, {}, null),
  document.getElementById('app')
);
