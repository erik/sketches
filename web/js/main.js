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

  asGeoJSON(index) {
    return featurePoint(this.point, {
      id: this.id,
      label: index.toString()
    });
  }
}

class Segment {
  static nextId = 0;

  constructor(from, to) {
    this.id = Segment.nextId++;
    this.from = from;
    this.to = to;

    this.reset();
  }

  reset() {
    this.state = 'start';
    this.geometry = [this.from.point, this.to.point];
  }

  async fetch() {
    this.state = 'load';

    if (this.inflight) {
      this.inflight.abort();
    }

    this.inflight = new AbortController();
    const res = await fetch('/route', {
      headers: { 'Content-Type': 'application/json' },
      method: 'POST',
      body: JSON.stringify({
        from: {
          lng: this.from.point.lng,
          lat: this.from.point.lat,
        },
        to: {
          lng: this.to.point.lng,
          lat: this.to.point.lat,
        }
      }),
      signal: this.inflight.signal,
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
    this.geometry = json.geometry;

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
    this.lastEvent = null;

    this.map = new maplibregl.Map({
      container: elem,
      style: 'https://tiles.stadiamaps.com/styles/outdoors.json',
      center: [12, 53],
      zoom: 4,
      hash: true,
    });

    // Add zoom and rotation controls to the map.
    this.map.addControl(new maplibregl.NavigationControl())
      .addControl(new MapControl(this, '🔙', () => this.popControlPoint()))
      .addControl(new MapControl(this, '⬇️', () => this.downloadGPX()))
      .on('load', () => this.attachSources())
      .on('click', 'control-points', (e)=> this.handleRemovePoint(e))
      .on('click', (e) => this.handleClick(e))
      // TODO: pull this out to helpers
      .on('mouseenter', 'route', (e) => {
        this.map.getCanvasContainer().style.cursor = 'grab';
        this.map.setLayoutProperty('drag-marker', 'visibility', 'visible');
        this.map.getSource('drag-marker').setData(featurePoint(e.lngLat));
      })
      .on('mousemove', 'route', (e) => {
        this.map.getSource('drag-marker').setData(featurePoint(e.lngLat));
      })
      .on('mouseleave', 'route', (e) => {
        if (this.map.getCanvasContainer().style.cursor !== 'grabbing')
          this.map.setLayoutProperty('drag-marker', 'visibility', 'none');
      })
      .on('mousedown', 'control-points', (e) => this.handleRepositionPoint(e))
      .on('mousedown', 'route', (e) => this.handleSplitSegment(e));
  }

  alreadyHandled(e) {
    if (this.lastEvent === e) {
      return true;
    }

    this.lastEvent = e;
    return false;
  }

  handleRemovePoint(e) {
    e.preventDefault();
    if (this.alreadyHandled(e)) return;

    const id = e.features[0].properties.id;
    const controlPoint = this.controlPoints.find(it => it.id === id);
    const index = this.controlPoints.indexOf(controlPoint);

    this.controlPoints.splice(index, 1);

    const newSegments = [];
    let prevPoint = null;

    for (let i = 0; i < this.segments.length; ++i) {
      const seg = this.segments[i];

      // a--1--b--2--c
      // delete a: remove segment 1
      // delete b: merge segment 1 and 2
      // delete c: remove segment 2
      if (seg.from === controlPoint) {
        if (prevPoint !== null) {
          newSegments.push(new Segment(prevPoint, seg.to));
        }
      } else if (seg.to === controlPoint) {
        prevPoint = seg.from;
      } else {
        newSegments.push(seg);
      }
    }

    this.segments = newSegments;
    this.reroute();
  }

  handleRepositionPoint(e) {
    e.preventDefault();
    if (this.alreadyHandled(e)) return;

    const id = e.features[0].properties.id;
    const controlPoint = this.controlPoints.find(it => it.id === id);
    const canvas = this.map.getCanvasContainer();

    canvas.style.cursor = 'grabbing';

    let moved = false;

    // TODO: be better at JS
    const that = this;
    function onMove(e) {
      moved = true;
      controlPoint.reposition(e.lngLat);
      that.setData();
    }

    function onFinish(e) {
      canvas.style.cursor = '';

      that.draggingPoint = false;
      that.map.off('mousemove', onMove);

      if (moved) {
        that.segments
          .filter(it => it.from === controlPoint || it.to === controlPoint)
          .forEach(it => it.reset());

        that.reroute();
      }
    }

    this.map
      .on('mousemove', onMove)
      .once('mouseup', onFinish);
  }

  handleSplitSegment(e) {
    e.preventDefault();

    if (this.alreadyHandled(e)) return;

    const segmentId = e.features[0].properties.id;
    const segment = this.segments.find(it => it.id === segmentId);
    const splitIndex = this.segments.indexOf(segment);

    const canvas = this.map.getCanvasContainer();

    canvas.style.cursor = 'grabbing';
    this.map.setLayoutProperty('drag-marker', 'visibility', 'visible');

    const splitMarkerSource = this.map.getSource('drag-marker');


    function onMove(e) {
      splitMarkerSource.setData(featurePoint(e.lngLat));
    }

    // TODO: be better at JS
    const that = this;
    function onFinish(e) {
      canvas.style.cursor = '';

      that.map.off('mousemove', onMove);
      that.map.setLayoutProperty('drag-marker', 'visibility', 'none');

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
      .addSource('drag-marker', { type: 'geojson', data: featurePoint({ lat: 0, lng: 0 }) })
      .addSource('elevation', {
        type: 'raster-dem',
        tiles: ['https://s3.amazonaws.com/elevation-tiles-prod/terrarium/{z}/{x}/{y}.png'],
        minzoom: 0,
        maxzoom: 15,
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
          'circle-stroke-width': 4,
          'circle-radius': 8,
        }
      })
      .addLayer({
        id: 'control-point-labels',
        type: 'symbol',
        source: 'control-points',
        layout: {
          'text-field': ['get', 'label'],
          'text-font': ['Open Sans Bold'],
          'text-size': 12,
        },
        paint: {
          'text-color': '#777',
        }
      })
      .addLayer({
        id: 'drag-marker',
        type: 'circle',
        source: 'drag-marker',
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
    const controlPoints = featureCollection(this.controlPoints.map((it, i) => it.asGeoJSON(i)));
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
    if (this.alreadyHandled(e)) return;

    const cp = new ControlPoint(e.lngLat);
    this.pushControlPoint(cp);
  }

  downloadGPX() {
    const trackPoints = this.segments
          .map(it => it.geometry.map(pt =>
            `<trkpt lat="${pt.lat}" lon="${pt.lng}"></trkpt>`)
          )
          .flat();
    const data = `\
<?xml version="1.0" encoding="UTF-8"?>
<gpx xmlns="http://www.topografix.com/GPX/1/1"
     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"
     creator="panamint"
     version="1.1">
  <trk>
    <name>panamint export</name>
    <trkseg>
        ${trackPoints.join('\n')}
    </trkseg>
  </trk>
</gpx>`;

    const element = document.createElement('a');
    element.setAttribute('href', 'data:application/gpx+xml,' + encodeURIComponent(data));
    element.setAttribute('download', 'route.gpx');

    element.style.display = 'none';
    document.body.appendChild(element);

    element.click();

    document.body.removeChild(element);
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

class MapControl {
  constructor(container, label, onClick) {
    this._container = container;
    this._label = label;
    this._onClick = onClick;
  }

  onAdd(map) {
    this._map = map;
    this._elem = document.createElement('div');
    this._elem.className = 'maplibregl-ctrl maplibregl-ctrl-group';
    this._elem.innerHTML = `<button>${this._label}</button>`;
    this._elem.onclick = () => this._onClick();
    return this._elem;
  }

  onRemove() {
    this._elem.parentNode.removeChild(this._elem);
    this._map = undefined;
  }
}
