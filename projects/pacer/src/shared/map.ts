import L, { LatLngExpression, LatLngTuple } from "leaflet";
import "leaflet/dist/leaflet.css";
import { RouteMarker, OldControlPoint } from "./index.js";

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
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" >
          <!-- Top-left corner bracket -->
          <path d="M 80 140 Q 80 80 140 80" fill="none" stroke="#000" stroke-width="40" stroke-linecap="round"/>

          <!-- Top-right corner bracket -->
          <path d="M 372 80 Q 432 80 432 140" fill="none" stroke="#000" stroke-width="40" stroke-linecap="round"/>

          <!-- Bottom-left corner bracket -->
          <path d="M 140 432 Q 80 432 80 372" fill="none" stroke="#000" stroke-width="40" stroke-linecap="round"/>

          <!-- Bottom-right corner bracket -->
          <path d="M 432 372 Q 432 432 372 432" fill="none" stroke="#000" stroke-width="40" stroke-linecap="round"/>

          <!-- Center rounded rectangle -->
          <rect x="160" y="140" width="192" height="232" rx="30" ry="30" fill="none" stroke="#000" stroke-width="32"/>
        </svg>
      </a>
    `;
    container.style.backgroundColor = "white";
    container.style.cursor = "pointer";
    container.title = "Recenter map";

    L.DomEvent.disableClickPropagation(container);
    container.onclick = () => this.controller.fitToContent();
    return container;
  }
}

export class MapController {
  map: L.Map;
  trackLayer: L.Polyline;
  userLocationMarker?: L.Marker;
  controls: L.Marker[];
  markers: L.Marker[];
  controlSegments: L.Layer[];

  constructor(map: L.Map) {
    this.map = map;

    this.trackLayer = null;
    this.controls = [];
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

  setTrack(
    coords: LatLngTuple[][],
    options: {
      fitBounds?: boolean;
      color?: string;
      weight?: number;
      opacity?: number;
    } = {},
  ) {
    if (this.trackLayer) {
      this.map.removeLayer(this.trackLayer);
    }

    if (coords.length === 0) return;

    const latLngs = coords.map((l) =>
      l.map(([lng, lat]) => [lat, lng]),
    ) as LatLngTuple[][];

    this.trackLayer = L.polyline(latLngs, {
      color: options.color || "#2563eb",
      weight: options.weight || 4,
      opacity: options.opacity || 0.7,
    }).addTo(this.map);

    if (options.fitBounds === true) {
      this.map.fitBounds(this.trackLayer.getBounds(), { padding: [100, 100] });
    }
  }

  setMarkers(markers: RouteMarker[]) {
    this.clearMarkers();

    for (const [index, marker] of markers.entries()) {
      let [lng, lat] = marker.coordinate;

      let icon = L.divIcon({
        className: "",
        html: `<div class="w-3 h-3 -translate-1/2 rounded-full bg-primary/50 tooltip hover:w-6 hover:h-6 transition-all border-2 border-primary drop-shadow-2xl" data-tip="${marker.name}"></div>`,
        iconSize: [12, 12],
        iconAnchor: [0, 0],
      });

      this.markers.push(
        L.marker({ lng, lat }, { icon })
          .addTo(this.map)
          .bindPopup(`<b>${marker.name}</b>`),
      );
    }
  }

  setControlPoints(
    controls: OldControlPoint[],
    options: {
      trackCoords?: any[];
      onClick?: (c: OldControlPoint) => void;
      onDragEnd?: (i: number, pt: L.LatLng, m: L.Marker) => void;
    } = {},
  ) {
    // Clear existing markers
    this.clearMarkers();

    for (const [index, cp] of controls.entries()) {
      const { lng, lat } = cp.coord;

      const name = cp.name || `CP ${index + 1}`;

      let icon = L.divIcon({
        className: "",
        html: `<div class="w-3 h-3 -translate-1/2 rounded-full bg-primary/50 tooltip hover:w-6 hover:h-6 transition-all border-2 border-primary drop-shadow-2xl" data-tip="${name}"></div>`,
        iconSize: [12, 12],
        iconAnchor: [0, 0],
      });

      const marker = L.marker([lat, lng], { icon })
        .addTo(this.map)
        .bindPopup(`<b>${name}</b>`);

      // Click handler for setup mode
      if (options.onClick) {
        marker.on("click", () => options.onClick(cp));
      }

      // Draggable in setup mode
      marker.dragging.enable();

      // Simple dragend handler - pass both index, coordinate, and marker
      marker.on("dragend", (e) => {
        if (options.onDragEnd) {
          options.onDragEnd(index, e.target.getLatLng(), marker);
        }
      });

      this.controls.push(marker);
    }

    // Draw dashed lines between checkpoints if either is not snapped to track
    for (let i = 0; i < controls.length - 1; i++) {
      const cp1 = controls[i];
      const cp2 = controls[i + 1];

      // Show dashed line if either checkpoint is not snapped to a track segment
      if (cp1.anchorSegmentId == null || cp2.anchorSegmentId == null) {
        const line = L.polyline([cp1.coord, cp2.coord], {
          color: "#666",
          weight: 2,
          opacity: 0.7,
          dashArray: "5, 10",
          interactive: false,
        }).addTo(this.map);

        this.controlSegments.push(line);
      }
    }
  }

  clearMarkers() {
    this.markers.forEach((marker) => this.map.removeLayer(marker));
    this.markers = [];

    this.controls.forEach((marker) => this.map.removeLayer(marker));
    this.controls = [];

    this.controlSegments.forEach((line) => this.map.removeLayer(line));
    this.controlSegments = [];
  }

  onMapClick(callback: (pt: L.LatLng) => void) {
    this.map.on("click", (e) => callback(e.latlng));
  }

  /**
   * Fit map to show all current layers.
   */
  fitToContent() {
    const bounds = L.latLngBounds([]);

    if (this.trackLayer) {
      bounds.extend(this.trackLayer.getBounds());
    }

    for (const marker of this.controls) {
      bounds.extend(marker.getLatLng());
    }

    this.map.fitBounds(bounds, { padding: [100, 100] });
  }
}

export function createMap(
  container: HTMLElement,
  options: {
    center?: LatLngExpression;
    zoom?: number;
  } = {},
) {
  // Initialize map
  const map = L.map(container, {
    center: options.center || [52.52, 13.405],
    zoom: options.zoom || 6,
    zoomControl: true,
  });

  // Add tile layer
  L.tileLayer(
    "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png",
    {
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
      maxZoom: 19,
    },
  ).addTo(map);

  const controller = new MapController(map);
  map.addControl(new RecenterMapControl(controller));
  return controller;
}
