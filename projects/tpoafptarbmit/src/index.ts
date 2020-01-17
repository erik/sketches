import * as L from 'leaflet';


const CALIFORNIA_DONUTS = {lat: 34.0688093, lng: -118.2930864};

const LAYERS = {
    OSM: L.tileLayer(
        'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
        {
            maxZoom: 19,
            attribution:
            '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        }
    ),
    'ESRI.WorldTopo': L.tileLayer(
        'https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
        {
            attribution:
            'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community'
        }
    ),
    'Open Topo': L.tileLayer(
        'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png',
        {
            maxZoom: 17,
            attribution:
            'Map data: &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, <a href="http://viewfinderpanoramas.org">SRTM</a> | Map style: &copy; <a href="https://opentopomap.org">OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a>)'
        }
    )
};

function fetchRoutes() {
    return fetch('/data/index.geojson')
        .then(res => res.json());
}

(function() {
    const donutShop = L.marker(CALIFORNIA_DONUTS, {
        icon: L.divIcon({ html: `<span class="text-3xl">🍩</span` })
    });

    const map = L.map('map', {
        center: CALIFORNIA_DONUTS,
        zoom: 14,
        layers: [LAYERS.OSM, donutShop],
    });

    L.control
        .layers(LAYERS, {
            'California Donuts': donutShop,
            // TODO: hide/show GeoJSON layer if possible
        })
        .addTo(map);

    fetchRoutes()
        .then(routes => L.geoJSON(routes, {
            onEachFeature: (feature, layer) => {
                const p = feature.properties;
                layer.bindPopup(`
<div class="route-info">
    <h1 class="text-xl font-bold">#${p.number} - ${p.name}</h1>
    <div class="text-sm">${p.description}</div>
`);
            }
        }).addTo(map));
})();
