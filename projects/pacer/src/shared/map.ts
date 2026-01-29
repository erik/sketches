import L, { latLngBounds, LatLngExpression, LatLngTuple } from "leaflet";
import "leaflet/dist/leaflet.css";
import { RouteMarker, Segment } from "./index.js";
import { htmlTemplate } from "../livewire.js";

const DARKMODE_TILES =
  "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png";
const LIGHTMODE_TILES =
  "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png";

class RecenterMapControl extends L.Control {
  controller: MapController;

  options: L.ControlOptions = {
    position: "topleft",
  };

  constructor(controller: MapController, options?: L.ControlOptions) {
    super(options);
    this.controller = controller;
  }

  onAdd(_map: L.Map) {
    const container = L.DomUtil.create(
      "div",
      "leaflet-bar leaflet-control leaflet-control-custom",
    );
    container.innerHTML = `
      <a href="#" role="button" title="Recenter map" style="width: 30px; height: 30px; display: flex; align-items: center; justify-content: center;">
        <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">
          <path fill="currentColor" d="M2.75 2.5a.25.25 0 0 0-.25.25v2.5a.75.75 0 0 1-1.5 0v-2.5C1 1.784 1.784 1 2.75 1h2.5a.75.75 0 0 1 0 1.5zM10 1.75a.75.75 0 0 1 .75-.75h2.5c.966 0 1.75.784 1.75 1.75v2.5a.75.75 0 0 1-1.5 0v-2.5a.25.25 0 0 0-.25-.25h-2.5a.75.75 0 0 1-.75-.75M1.75 10a.75.75 0 0 1 .75.75v2.5c0 .138.112.25.25.25h2.5a.75.75 0 0 1 0 1.5h-2.5A1.75 1.75 0 0 1 1 13.25v-2.5a.75.75 0 0 1 .75-.75m12.5 0a.75.75 0 0 1 .75.75v2.5A1.75 1.75 0 0 1 13.25 15h-2.5a.75.75 0 0 1 0-1.5h2.5a.25.25 0 0 0 .25-.25v-2.5a.75.75 0 0 1 .75-.75M8 10a2 2 0 1 0 .001-3.999A2 2 0 0 0 8 10"/>
          <path fill="currentColor" d="M8 10a2 2 0 1 0 .001-3.999A2 2 0 0 0 8 10"/>
        </svg>
      </a>
    `;
    container.style.backgroundColor = "white";
    container.style.cursor = "pointer";
    container.title = "Recenter map";

    L.DomEvent.disableClickPropagation(container);
    container.onclick = (e) => {
      e.preventDefault();
      this.controller.fitToContent();
    };
    return container;
  }
}

export class MapController {
  map: L.Map;
  trackLayers: L.Polyline[];
  userLocationMarker?: L.Marker;
  markers: L.Marker[];
  controlSegments: L.Layer[];
  tileLayer: L.TileLayer;

  constructor(map: L.Map, tileLayer: L.TileLayer) {
    this.map = map;
    this.tileLayer = tileLayer;

    this.trackLayers = [];
    this.markers = [];
    this.controlSegments = [];
  }

  getMap() {
    return this.map;
  }

  setUserLocation(location: LatLngExpression) {
    if (!this.userLocationMarker) {
      this.userLocationMarker = L.marker(location, {
        icon: L.divIcon({
          className: "user-location-marker",
          html: `<div class="w-3 h-3 rounded-full bg-blue-600 border-2 border-white animate-pulse"></div>`,
          iconSize: [12, 12],
          iconAnchor: [6, 6],
        }),
      }).addTo(this.map);
    } else {
      this.userLocationMarker.setLatLng(location);
    }
  }

  setTrackSegments(
    segments: Segment[],
    options: {
      fitBounds?: boolean;
      color?: string;
      weight?: number;
      opacity?: number;
    } = {},
  ) {
    for (const layer of this.trackLayers) {
      this.map.removeLayer(layer);
    }

    for (const segment of segments) {
      const coords = segment.geometry.coordinates.map(
        ([lng, lat]) => [lat, lng] as LatLngTuple,
      );

      const layer = L.polyline(coords, {
        color: options.color || "#2563eb",
        weight: options.weight || 4,
        opacity: options.opacity || 0.7,
      })
        .bindPopup(`<h1>${segment.title}</h1>`)
        .addTo(this.map);

      this.trackLayers.push(layer);
    }

    if (segments.length !== 0 && options.fitBounds === true) {
      this.fitToContent();
    }
  }

  setRouteMarkers(
    markers: RouteMarker[],
    options: {
      onClick?: (m: RouteMarker) => void;
      onDrag?: (i: number, pt: L.LatLng, m: L.Marker) => void;
    } = {},
  ) {
    this.clearMarkers();

    for (const [index, marker] of markers.entries()) {
      let [lng, lat] = marker.coordinate;
      const markerName = marker.name || `CP ${index + 1}`;

      let icon = L.divIcon({
        className: "",
        html: `<div class="w-3 h-3 -translate-1/2 rounded-full bg-primary/50 tooltip hover:w-6 hover:h-6 transition-all border-2 border-primary drop-shadow-2xl" data-tip="${markerName}"></div>`,
        iconSize: [12, 12],
        iconAnchor: [0, 0],
      });

      const mapMarker = L.marker({ lng, lat }, { icon }).addTo(this.map);

      this.markers.push(mapMarker);

      if (options.onClick) {
        mapMarker.on("click", () => options.onClick(marker));
      }

      if (options.onDrag) {
        mapMarker.dragging.enable();
        mapMarker.on("dragend", (e) =>
          options.onDrag(index, e.target.getLatLng(), mapMarker),
        );
      }
    }
  }

  // TODO: port this
  // Draw dashed lines between checkpoints if either is not snapped to track
  // for (let i = 0; i < controls.length - 1; i++) {
  //   const cp1 = controls[i];
  //   const cp2 = controls[i + 1];
  //   // Show dashed line if either checkpoint is not snapped to a track segment
  //   if (cp1.anchorSegmentId == null || cp2.anchorSegmentId == null) {
  //     const line = L.polyline([cp1.coord, cp2.coord], {
  //       color: "#666",
  //       weight: 2,
  //       opacity: 0.7,
  //       dashArray: "5, 10",
  //       interactive: false,
  //     }).addTo(this.map);
  //     this.controlSegments.push(line);
  //   }
  // }

  clearMarkers() {
    this.markers.forEach((marker) => this.map.removeLayer(marker));
    this.markers = [];

    this.controlSegments.forEach((line) => this.map.removeLayer(line));
    this.controlSegments = [];
  }

  onMapClick(callback: (pt: L.LatLng) => void) {
    this.map.on("click", (e) => callback(e.latlng));
  }

  fitToContent() {
    const bounds = this.trackLayers.reduce(
      (bbox, layer) => bbox.extend(layer.getBounds()),
      L.latLngBounds([]),
    );

    for (const marker of this.markers) {
      bounds.extend(marker.getLatLng());
    }

    this.map.fitBounds(bounds, { padding: [100, 100] });
  }

  setDarkMode(darkmode: boolean) {
    this.tileLayer.setUrl(darkmode ? DARKMODE_TILES : LIGHTMODE_TILES);
  }
}

export function createMap(
  container: HTMLElement,
  options: {
    center?: LatLngExpression;
    zoom?: number;
    darkmode?: boolean;
  } = {},
) {
  const map = L.map(container, {
    center: options.center || [52.52, 13.405],
    zoom: options.zoom || 6,
    zoomControl: true,
  });

  const tileLayer = L.tileLayer(DARKMODE_TILES, {
    attribution:
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
    maxZoom: 19,
  }).addTo(map);

  const controller = new MapController(map, tileLayer);
  map.addControl(new RecenterMapControl(controller));
  controller.setDarkMode(options.darkmode ?? true);
  return controller;
}
