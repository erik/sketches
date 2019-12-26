"use strict";

import React, { createElement as h, useState, useCallback, useEffect } from 'react';
import ReactDOM from 'react-dom';
import { useDropzone } from 'react-dropzone';
import * as Leaflet from 'leaflet';
import leafletImage from 'leaflet-image';

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

// TODO: Currently treating Leaflet as a blackbox inside of React. Is
// this how to do this? Shouldn't React maintain all of this state?
//
// TODO: Possibly would be better to use GeoJSON instead of our own
// representation so native Leaflet functionality can be used.
//
// TODO: Layer controls: https://leafletjs.com/examples/layers-control/example.html
const RenderOptionsScreen = ({switchScreens, data}) => {
  const mapRef = React.useRef(null);
  const [panels, setPanels] = useState([]);
  const [renders, setRenders] = useState([]);
  const [route, setRoute] = useState(null);
  const [waypoints, setWaypoints] = useState(null);


  useEffect(() => {
    const points = data.route.routePoints.map(it => [it.latitude, it.longitude]);
    const route = new Leaflet.polyline(points, {noClip: true});
    setRoute(route);

    const wpts = data.route.wayPoints
          .map(it => new Leaflet.marker([
            it.latitude,
            it.longitude
          ], {title: it.name}).bindTooltip(it.name));
    setWaypoints(wpts);

    mapRef.current = new Leaflet.map('map', {
      center: [0, 0],
      zoom: 13,
      preferCanvas: true,
      layers: [
        ...wpts,
        route,
        // TODO: This should be customizable
        new Leaflet.TileLayer(
          'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 19,
            attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
          }),
      ]
    });

    // Zoom to the route.
    mapRef.current.fitBounds(route.getBounds());
  }, []);

  const addPanel = () => {
    const bounds = mapRef.current.getBounds().pad(-0.4);

    const width = Math.abs(bounds.getEast() - bounds.getWest());
    const height = Math.abs(bounds.getNorth() - bounds.getSouth());

    const index = panels.length;

    // create an orange rectangle
    const rect = Leaflet.rectangle(bounds, {color: "#ff7800", weight: 1})
          .bindTooltip(`Map Panel ${index+1}`, {permanent: true});

    rect.on({
      mousedown: () => {
        rect.closeTooltip();
        mapRef.current.dragging.disable();
        mapRef.current.on('mousemove', (e) => {
          const bounds = [
            [e.latlng.lat - height / 2, e.latlng.lng - width / 2],
            [e.latlng.lat + height / 2, e.latlng.lng + width / 2],
          ];
          rect.setBounds(bounds);
        });

        mapRef.current.on('mouseup', () => {
          mapRef.current.dragging.enable();
          mapRef.current.removeEventListener('mousemove');
          rect.openTooltip();
        });
      },
    });

    rect.addTo(mapRef.current);

    panels.push({
      rect,
      zoomLevel: mapRef.current.getZoom(),
    });
    setPanels(panels);
  };

  const removePanel = () => {
    if (panels.length === 0)
      return;

    const panel = panels.pop();
    panel.rect.remove();

    setPanels(panels);
  };

  const renderMap = async () => {
    const renders = [];

    const bounds = panels.map(p => p.rect.getBounds());
    const centers = panels.map(p => p.rect.getCenter());

    // Pop open all tooltips
    for (const wpt of waypoints) {
      wpt.openTooltip();
    }

    // First, make all the panels invisible
    for (const panel of panels) {
      panel.rect.remove();
    }

    const dim = mapRef.current.getSize();

    const renderPanel = (p) => new Promise((resolve, reject) => {
      const b = bounds.shift();
      const c = centers.shift();
      console.log('inspecting: ', p, b);

      mapRef.current.fitBounds(b);
      mapRef.current.setView(c, mapRef.current.getZoom());

      // We do this within a setTimeout so that the map has a chance
      // to rerender before we take the snapshot.
      //
      // It's real slow anyway :(
      //
      // TODO: Maybe there's an event we can listen to instead?
      setTimeout(() => {
        console.log('ready!')
        leafletImage(mapRef.current, (err, canvas) => {
          if (err) {
            reject(err);
            return;
          }

          canvas.toBlob(blob => {
            resolve({
              src: URL.createObjectURL(blob),
              width: dim.x,
              height: dim.y,
            });
          });
        });
      }, 1000);
    });

    for (const panel of panels) {
      const render = await renderPanel(panel);
      renders.push(render);
    }

    // Make them visible again
    for (const panel of panels) {
      panel.rect.addTo(mapRef.current);
    }

    mapRef.current.fitBounds(route.getBounds());
    setRenders(renders);
  };

  // FIXME: Styling is uhhh bad.
  return h('div', {}, [
    h('h1', {}, data.route.name),
    h('div', {id: 'map', style: {height: '900px'}}, null),
    h('div', {}, [
      h('button', {type: 'button', className: 'btn', onClick: addPanel}, 'Add Panel'),
      h('button', {type: 'button', className: 'btn btn-danger', onClick: removePanel}, 'Remove Panel'),
      h('button', {type: 'button', className: 'btn btn-primary', onClick: renderMap}, 'Render Map')
    ],
      h('div', {}, renders.map(it => h('img', it)))),
    ]);
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
