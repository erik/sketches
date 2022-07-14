const featureLine = (path, props) => ({
  type: 'Feature',
  geometry: {
    type: 'LineString',
    coordinates: path.map(it => [it.lng, it.lat]),
  },
  properties: props,
});

const featurePoint = (pt, props) => ({
  type: 'Feature',
  geometry: {
    type: 'Point',
    coordinates: [pt.lng, pt.lat],
  },
  properties: props,
});

const featureCollection = (features = []) => ({
  type: 'FeatureCollection',
  features,
});

class ControlPoint {
  static nextId = 0;

  constructor(pt) {
    this.id = ControlPoint.nextId++;
    this.point = pt;
  }

  reposition(pt) {
    this.point = pt;
  }

  asGeoJSON() {
    return featurePoint(this.point, {});
  }
}

class Segment {
  static nextId = 0;

  constructor(from, to) {
    this.id = Segment.nextId++;
    this.from = from;
    this.to = to;

    this.state = 'start';
    this.geometry = [this.from.point, this.to.point];
  }

  async fetch() {
    this.state = 'load';

    const res = await fetch('/route', {
      headers: { 'Content-Type': 'application/json' },
      method: 'POST',
      body: JSON.stringify({
        from: {
          lon: this.from.point.lng,
          lat: this.from.point.lat,
        },
        to: {
          lon: this.to.point.lng,
          lat: this.to.point.lat,
        }
      }),
    }).catch(err => {
        console.error('something went wrong', err);
        this.state = 'error';
    });

    const json = await res.json();
    console.log('routing response', json);

    if (json === null) {
      this.state = 'no_route';
      return;
    }

    this.state = 'done';
    this.geometry = json.geometry.map(it => ({
      lng: it.lon,
      lat: it.lat,
    }));

    if (this.geometry.length >= 2) {
      this.from.reposition(this.geometry[0]);
      this.to.reposition(this.geometry[this.geometry.length - 1]);
    }
  }

  splitAt(cp) {
    return [
      new Segment(this.from, cp),
      new Segment(cp, this.to),
    ];
  }

  asGeoJSON() {
    const style = {
      'start': {
        color: '#f3a',
        width: 3,
      },
      'load': {
        color: '#31a',
        width: 3,
      },
      'no_route': {
        color: '#f21',
      },
      'error': {
        color: '#f21',
      },
      'done': {
        color: '#a3f',
      }
    };

    return featureLine(this.geometry, {
      id: this.id,
      width: 5,
      ...style[this.state],
    });
  }
}

export class MapContainer {
  constructor(elem) {
    this.controlPoints = [];
    this.segments = [];

    this.map = new maplibregl.Map({
      container: elem,
      style: 'https://tiles.stadiamaps.com/styles/outdoors.json',
      center: [12, 53],
      zoom: 4,
      hash: true,
    });

    // Add zoom and rotation controls to the map.
    this.map.addControl(new maplibregl.NavigationControl())
      .addControl(new UndoMapControl(this))
      .on('load', () => this.attachSources())
      .on('click', (e) => this.handleClick(e))
      // TODO: pull this out to helpers
      .on('mouseenter', 'route', (e) => {
        this.map.getCanvasContainer().style.cursor = 'grab';
        this.map.setLayoutProperty('segment-split-marker', 'visibility', 'visible');
        this.map.getSource('segment-split-marker').setData(featurePoint(e.lngLat));
      })
      .on('mousemove', 'route', (e) => {
        this.map.getSource('segment-split-marker').setData(featurePoint(e.lngLat));
      })
      .on('mouseleave', 'route', (e) => {
        if (this.map.getCanvasContainer().style.cursor !== 'grabbing')
          this.map.setLayoutProperty('segment-split-marker', 'visibility', 'none');
      })
      .on('mousedown', 'route', (e) => { this.handleSplitSegment(e); });

  }

  handleSplitSegment(e) {
    e.preventDefault();

    const segmentId = e.features[0].properties.id;
    const segment = this.segments.find(it => it.id === segmentId);
    const splitIndex = this.segments.indexOf(segment);

    const canvas = this.map.getCanvasContainer();

    canvas.style.cursor = 'grabbing';
    this.map.setLayoutProperty('segment-split-marker', 'visibility', 'visible');

    const splitMarkerSource = this.map.getSource('segment-split-marker');


    function onMove(e) {
      splitMarkerSource.setData(featurePoint(e.lngLat));
    }

    // TODO: be better at JS
    const that = this;
    function onFinish(e) {
      canvas.style.cursor = '';

      that.map.off('mousemove', onMove);
      that.map.setLayoutProperty('segment-split-marker', 'visibility', 'none');

      const point = new ControlPoint(e.lngLat);
      that.controlPoints.splice(splitIndex + 1, 0, point);

      that.segments = [
        ...that.segments.slice(0, splitIndex),
        ...segment.splitAt(point),
        ...that.segments.slice(splitIndex + 1),
      ];

      that.reroute();
    }

    this.map
      .on('mousemove', onMove)
      .once('mouseup', onFinish);

  }

  attachSources() {
    this.map
      .addSource('control-points', { type: 'geojson', data: featureCollection() })
      .addSource('segments', { type: 'geojson', data: featureCollection() })
      .addSource('segment-split-marker', { type: 'geojson', data: featurePoint({ lat: 0, lng: 0 }) })
      .addSource('elevation', {
        type: 'raster-dem',
        tiles: [
          'https://s3.amazonaws.com/elevation-tiles-prod/terrarium/{z}/{x}/{y}.png',
        ],
        minzoom: 0,
        maxzoom: 16,
        tileSize: 256,
        encoding: 'terrarium',
      })
      .addLayer({
        id: 'elevation',
        type: 'hillshade',
        source: 'elevation',
        paint: {
          "hillshade-shadow-color": "hsl(39, 21%, 33%)",
          "hillshade-illumination-direction": 315,
          "hillshade-exaggeration": 0.3
        }
      })
      .addLayer({
        id: 'route-outline',
        type: 'line',
        source: 'segments',
        layout: {'line-join': 'round', 'line-cap': 'round'},
        paint: {'line-color': '#fff', 'line-width': 12}
      })
      .addLayer({
        id: 'route',
        type: 'line',
        source: 'segments',
        layout: {
          'line-join': 'round',
          'line-cap': 'round'
        },
        paint: {
          'line-dasharray': [1, 2],
          'line-color': ['get', 'color'],
          'line-width': ['get', 'width'],
        }
      })
      .addLayer({
        id: 'control-points',
        type: 'circle',
        source: 'control-points',
        paint: {
          'circle-color': '#fff',
          'circle-stroke-color': '#a3f',
          'circle-stroke-width': 6,
          'circle-radius': 6,
        }
      })
      .addLayer({
        id: 'segment-split-marker',
        type: 'circle',
        source: 'segment-split-marker',
        layout: {
          visibility: 'none',
        },
        paint: {
          'circle-color': '#fff',
          'circle-stroke-color': '#f3f',
          'circle-stroke-width': 5,
          'circle-radius': 5,
        }
      });
    ;
  }

  setData() {
    const controlPoints = featureCollection(this.controlPoints.map(it => it.asGeoJSON()));
    const segments = featureCollection(this.segments.map(it => it.asGeoJSON()));

    this.map.getSource('control-points')
      .setData(controlPoints);
    this.map.getSource('segments')
      .setData(segments);
  }

  reroute() {
    this.setData();

    const promises = this.segments
          .filter(it => it.state === 'start')
          .map(it => it.fetch());

    Promise.allSettled(promises)
      .then(() => this.setData());
  }

  handleClick(e) {
    const cp = new ControlPoint(e.lngLat);
    this.pushControlPoint(cp);
  }

  pushControlPoint(cp) {
    this.controlPoints.push(cp);
    if (this.controlPoints.length > 1) {
      const from = this.controlPoints[this.controlPoints.length - 2];
      const segment = new Segment(from, cp);

      this.pushSegment(segment);
    }
    this.reroute();
  }

  pushSegment(segment) {
    this.segments.push(segment);
  }

  popControlPoint() {
    if (this.controlPoints.length === 0) return;

    this.controlPoints.pop();
    if (this.segments.length > 0) this.segments.pop();

    this.reroute();
  }
}

class UndoMapControl {
  constructor(container) {
    this._container = container;
  }

  onAdd(map) {
    this._map = map;
    this._elem = document.createElement('button');
    this._elem.className = 'maplibregl-ctrl';
    this._elem.textContent = '🔙';
    this._elem.onclick = () => this.onClick();
    return this._elem;
  }

  onClick() {
    this._container.popControlPoint();
  }


  onRemove() {
    this._elem.parentNode.removeChild(this._elem);
    this._map = undefined;
  }
}
