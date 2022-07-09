export class MapContainer {
  constructor(elem) {
    this.segments = [];
    this.controlPoints = [];
    this.lastPoint = null;

    this.map = new mapboxgl.Map({
      container: elem,
      style: 'https://tiles.stadiamaps.com/styles/outdoors.json',
      center: [12, 53],
      zoom: 4
    });

    // Add zoom and rotation controls to the map.
    this.map.addControl(new mapboxgl.NavigationControl());
    this.map.on('load', () => this.attachSources());
    this.map.on('click', (e) => this.handleClick(e));
  }

  attachSources() {
    this.map.addSource('points', {
      type: 'geojson',
      data: {
        type: 'Feature',
        properties: {},
        geometry: {
          type: 'LineString',
          coordinates: []
        }
      }
    });

    this.map.addLayer({
      id: 'route-outline',
      type: 'line',
      source: 'points',
      layout: {'line-join': 'round', 'line-cap': 'round'},
      paint: {'line-color': '#fff', 'line-width': 8}
    });

    this.map.addLayer({
      id: 'route',
      type: 'line',
      source: 'points',
      layout: {'line-join': 'round', 'line-cap': 'round'},
      paint: {'line-color': '#a3f', 'line-width': 3}
    });
  }

  addLineSegment(points) {
    this.segments.push(points);

    const data = {
      type: 'Feature',
      properties: {},
      geometry: {
        type: 'LineString',
        coordinates: this.segments.flat().map(({lat, lon}) => [lon, lat])
      }
    };

    console.log('Setting data to ', data);
    this.map.getSource('points')
      .setData(data);
  }

  handleClick(e) {
    let point = {
      lat: e.lngLat.lat,
      lon: e.lngLat.lng,
    };

    let marker = new mapboxgl.Marker()
        .setLngLat([point.lon, point.lat])
        .addTo(this.map);

    this.controlPoints.push(marker);

    if (this.lastPoint !== null) {
      fetch('/route', {
        headers: { 'Content-Type': 'application/json' },
        method: 'POST',
        body: JSON.stringify({
          from: this.lastPoint,
          to: point
        }),
      })
        .then(res => res.json())
        .then(res => {
          console.log('Route response: ', res);
          if (res.route) { this.addLineSegment(res.route); }
          else { marker.remove(); }
        })
        .catch(err => console.error(err));
    }

    this.lastPoint = point;
  }
}
