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

    // Track each route individually rather than adding the entire geoJson
    // object to the map at once.
    const routeFeatures: L.LayerGroup[] = [];

    const highlightLayer = (layer: L.FeatureGroup) => {
        previousRoute?.closePopup();
        previousRoute?.setStyle(routeStyles.base);
        previousRoute = layer;

        layer.bringToFront();
        layer.setStyle(routeStyles.highlight);
        layer.openPopup();
    };

    const createGeoJsonLayer = (routes: GeoJson.FeatureCollection) => L.geoJSON(routes, {
        style: routeStyles.base,
        onEachFeature: (feature, layer: L.FeatureGroup) => {
            const lg: L.LayerGroup = L.layerGroup()
                .addLayer(
                    layer
                        .bindPopup(popupForFeature(feature.properties))
                        .on('click', () => highlightLayer(layer)))
                .addTo(map);

            routeFeatures.push(lg);
        }
    });

    fetchRoutes().then(routes => {
        const geoJson = createGeoJsonLayer(routes);

        const routeColumns = document.querySelectorAll('.route-col');
        const perColumn = Math.round(routeFeatures.length / routeColumns.length);
        let colIndex = 0;

        routes.features.reverse().forEach((route, i) => {
            const props = route.properties;

            const node = document.createElement('div');
            node.setAttribute('class', 'route-item cursor-pointer hover-underline');
            node.appendChild(document.createTextNode(`#${props?.number??''} - ${props?.name?? 'Unnamed'}`));
            node.onclick = () => {
                // TODO: Popup opens in a weird location.
                // Since we're reversed we have to look from the end
                const group = routeFeatures[routeFeatures.length - 1 - i];
                // TODO: the "eachLayer" thing is pretty gross.
                group.eachLayer((l: L.Layer) => {
                    const lg = <L.FeatureGroup>l;
                    map.fitBounds(lg.getBounds());
                    highlightLayer(lg);
                });

                // Back to the top
                window.scrollTo(0, 0);
            };

            routeColumns[colIndex]?.appendChild(node);
            if (i >= (1+colIndex) * perColumn) {
                colIndex++;
            }
        });
    });
})();
