let points = [];
let map = new mapboxgl.Map({
  container: 'map',
  style: 'https://tiles.stadiamaps.com/styles/outdoors.json',
  center: [12, 53],
  zoom: 4
});

map.on('load', () => {
  map.addSource('points', {
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

  map.addLayer({
    id: 'route-outline',
    type: 'line',
    source: 'points',
    layout: {'line-join': 'round', 'line-cap': 'round'},
    paint: {'line-color': '#fff', 'line-width': 8}
  });

  map.addLayer({
    id: 'route',
    type: 'line',
    source: 'points',
    layout: {'line-join': 'round', 'line-cap': 'round'},
    paint: {'line-color': '#a3f', 'line-width': 3}
  });
});

function drawLine(points) {
  const data = {
    type: 'Feature',
    properties: {},
    geometry: {
      type: 'LineString',
      coordinates: points.map(({lat, lon}) => [lon, lat])
    }
  };
  console.log('Setting data to ', data);
  map.getSource('points')
    .setData(data);
}

// Add zoom and rotation controls to the map.
map.addControl(new mapboxgl.NavigationControl());

map.on('click', (e) => {
  let {lat, lng} = e.lngLat;
  points.push({lat, lon: lng});
  if (points.length === 2) {
    let [from, to] = points;
    points = [];
    fetch('/route', {
      headers: { 'Content-Type': 'application/json' },
      method: 'POST',
      body: JSON.stringify({from, to}),
    })
      .then(res => res.json())
      .then(res => {
        console.log('res: ', res);
        if (res.route) {
          drawLine(res.route);
        }
      })
      .catch(err => console.error(err));
  }
});
