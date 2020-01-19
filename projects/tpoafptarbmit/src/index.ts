import * as L from 'leaflet';
import * as GeoJson from 'geojson';


const CALIFORNIA_DONUTS = {lat: 34.0688093, lng: -118.2930864};

const LAYERS: {[key: string]: L.TileLayer} = {
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
        }),
    'Stamen': L.tileLayer(
        'https://stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}{r}.png', {
            attribution: 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
            subdomains: 'abcd',
            minZoom: 0,
            maxZoom: 20,
        })
};

function fetchRoutes(): Promise<GeoJson.FeatureCollection> {
    return fetch('/data/index.geojson')
        .then(res => res.json())
        .then(json => <GeoJson.FeatureCollection>json);
}

(function() {
    let geoJson: L.GeoJSON;

    const donutShop = L.marker(CALIFORNIA_DONUTS, {
        icon: L.divIcon({
            className: 'donut-shop-icon',
            html: `<span class="text-3xl">🍩</span`
        })
    });

    const map = L.map('map', {
        center: CALIFORNIA_DONUTS,
        zoom: 13,
        layers: [LAYERS['Stamen'], donutShop],
    });

    L.control
        .layers(LAYERS, {
            'California Donuts': donutShop,
            // TODO: hide/show GeoJSON layer if possible
        })
        .addTo(map);

    // TODO: Link out to geojson.io for full resolution map.
    const popupForFeature = (props: GeoJson.GeoJsonProperties) => `
<div class="route-info">
<h1 class="text-xl font-bold">#${props?.number ?? ''} - ${props?.name ?? 'Unnamed'}</h1>
<div class="text-sm">${props?.description ?? 'No description.'}</div>
</div>`;

    const routeStyles: {[key: string]: L.PathOptions} = {
        base: {
            color: '#A0C8D8',
            weight: 2,
        },
        background: {
            color: '#A0C8D83F',
            weight: 2,
        },
        highlight: {
            color: '#B61326',
            weight: 3,
        },
    };

    // Last route we had highlighted, so that it can be cleared.
    let previousRoute: L.FeatureGroup | null;

    const createGeoJsonLayer = (routes: GeoJson.FeatureCollection) => L.geoJSON(routes, {
        style: routeStyles.base,
        onEachFeature: (feature, layer: L.FeatureGroup) => {
            layer.bindPopup(popupForFeature(feature.properties));
            layer.on('click', () => {
                previousRoute?.setStyle(routeStyles.base);
                previousRoute = layer;

                layer.bringToFront();
                layer.setStyle(routeStyles.highlight);
            });
        }
    });

    fetchRoutes().then(routes => {
        geoJson = createGeoJsonLayer(routes).addTo(map);
    });
})();
