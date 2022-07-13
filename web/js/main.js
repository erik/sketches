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
    this.splitMarker = new mapboxgl.Marker({color: '#fff'});
    this.hoveredSegment = null;

    this.map = new mapboxgl.Map({
      container: elem,
      style: 'https://tiles.stadiamaps.com/styles/outdoors.json',
      center: [12, 53],
      zoom: 4,
      hash: true,
    });

    // Add zoom and rotation controls to the map.
    this.map.addControl(new mapboxgl.NavigationControl())
      .addControl(new UndoMapControl(this))
      .on('load', () => this.attachSources())
      .on('click', (e) => this.handleClick(e))
    // TODO: refactor this. not really how it's meant to work.
      .on('mousemove', 'route', (e) => this.handleMouseMove(e))
      .on('mouseleave', 'route', (e) => this.handleMouseMove(e))
      .on('mousedown', 'route', (e) => {
        e.preventDefault();
        this.map.getCanvasContainer().style.cursor = 'grabbing';

        // FIXME: this is shit
        const cb = this.handleDragSplit.bind(this);
        function handler(e) { return cb(e); }

        this.map
          .on('mousemove', handler)
          .once('mouseup', (e) => {
            this.map.off('mousemove', handler);
            this.handleDragSplitEnd(e);
          });
      });

  }

  handleDragSplit(e) {
    if (this.hoveredSegment === null) return;

    this.splitMarker.setLngLat(e.lngLat);
  }

  handleDragSplitEnd(e) {
    if (this.hoveredSegment === null) return;
    this.map.getCanvasContainer().style.cursor = '';

    const point = new ControlPoint(e.lngLat);
    const splitIndex = this.segments.indexOf(this.hoveredSegment);

    this.controlPoints.splice(splitIndex + 1, 0, point);

    this.segments = [
      ...this.segments.slice(0, splitIndex),
      ...this.hoveredSegment.splitAt(point),
      ...this.segments.slice(splitIndex + 1),
    ];

    this.splitMarker.remove();
    this.hoveredSegment = null;

    this.reroute();
  }

  handleMouseMove(e) {
    if (this.segments.length === 0) return;

    const items = this.map.queryRenderedFeatures(e.point, {layers: ['route-outline']});
    if (items.length === 0) {
      this.splitMarker.remove();
      this.hoveredSegment = null;
      return;
    }

    this.hoveredSegment = this.segments.find(it => it.id === items[0].properties.id);

    this.splitMarker
      .setLngLat(e.lngLat)
      .addTo(this.map);
  }

  attachSources() {
    this.map
      .addSource('control-points', { type: 'geojson', data: featureCollection() })
      .addSource('segments', { type: 'geojson', data: featureCollection() })
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
        layout: {'line-join': 'round', 'line-cap': 'round'},
        paint: {
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
    this._elem.className = 'mapboxgl-ctrl';
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
