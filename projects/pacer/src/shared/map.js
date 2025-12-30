/**
 * Map visualization for tracks and checkpoints using Leaflet.
 */

import L from "leaflet";
import "leaflet/dist/leaflet.css";
import { snapToNearestTrackSegment } from "./geo.js";

export function createMap(container, options = {}) {
  // Initialize map
  const map = L.map(container, {
    center: options.center || [52.52, 13.405], // Default to Berlin
    zoom: options.zoom || 6,
    zoomControl: true,
  });

  // Add tile layer
  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution:
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
    maxZoom: 19,
  }).addTo(map);

  // Track state
  let trackLayer = null;
  let checkpointMarkers = [];
  let checkpointLines = [];

  // Add custom recenter control
  const recenterControl = L.Control.extend({
    options: {
      position: "topleft",
    },

    onAdd: function (map) {
      const container = L.DomUtil.create(
        "div",
        "leaflet-bar leaflet-control leaflet-control-custom",
      );
      container.innerHTML = `<span class="p text-xs self-baseline">x</span>`;
      container.style.backgroundColor = "white";
      container.style.width = "30px";
      container.style.height = "30px";
      container.style.lineHeight = "30px";
      container.style.textAlign = "center";
      container.style.fontSize = "20px";
      container.style.cursor = "pointer";
      container.style.fontWeight = "bold";
      container.title = "Recenter map";

      container.onclick = function () {
        mapController.fitToContent();
      };

      return container;
    },
  });

  map.addControl(new recenterControl());

  const mapController = {
    /**
     * Get the underlying Leaflet map instance
     */
    getMap() {
      return map;
    },

    /**
     * Display a track on the map.
     * @param {[number, number][][]} coords - Array of [lng, lat] pairs
     * @param {Object} options - Styling options
     */
    showTrack(coords, options = {}) {
      // Remove existing track
      if (trackLayer) {
        map.removeLayer(trackLayer);
      }

      if (coords.length === 0) return;

      // Convert [lng, lat] to [lat, lng] for Leaflet
      const latLngs = coords.map((l) => l.map(([lng, lat]) => [lat, lng]));

      trackLayer = L.polyline(latLngs, {
        color: options.color || "#2563eb",
        weight: options.weight || 4,
        opacity: options.opacity || 0.7,
      }).addTo(map);

      // Fit bounds to track
      if (options.fitBounds === true) {
        map.fitBounds(trackLayer.getBounds(), { padding: [100, 100] });
      }
    },

    /**
     * Display checkpoints on the map.
     * @param {Array} controls - Checkpoint objects with {id, name, coord, km}
     * @param {Object} options - Display options
     * @param {[number, number][][]} [options.trackCoords] - Track coordinates for snapping markers to track
     */
    showCheckpoints(controls, options = {}) {
      // Clear existing markers
      this.clearControls();

      controls.forEach((cp, index) => {
        const { lng, lat } = cp.coord;

        const name = cp.name || `CP ${index + 1}`;

        // Create custom icon for start/finish
        let icon = L.divIcon({
          className: "",
          html: `<div class="w-3 h-3 -translate-1/2 rounded-full bg-primary/50 tooltip hover:w-6 hover:h-6 transition-all border-2 border-primary drop-shadow-2xl" data-tip="${name}"></div>`,
          iconSize: [12, 12],
          iconAnchor: [0, 0],
        });

        const marker = L.marker([lat, lng], { icon })
          .addTo(map)
          .bindPopup(`<b>${name}</b><br>${cp.km?.toFixed(1)} km$`);

        // Click handler for setup mode
        if (options.onClick) {
          marker.on("click", () => options.onClick(cp));
        }

        // Draggable in setup mode
        marker.dragging.enable();

        // Visually keep track of previous position
        let prevPositionMarker = null;
        marker.on("dragstart", (e) => {
          // Create ghost marker at original position
          prevPositionMarker = L.marker(e.target.getLatLng(), {
            icon: L.divIcon({
              className: "",
              html: `<div class="w-3 h-3 rounded-full bg-primary/50"></div>`,
              iconSize: [12, 12],
              iconAnchor: [6, 6],
            }),
            interactive: false,
          }).addTo(map);
        });

        marker.on("dragend", (e) => {
          if (prevPositionMarker) {
            map.removeLayer(prevPositionMarker);
            prevPositionMarker = null;
          }

          // Snap to nearest track point if track coordinates are provided
          if (options.trackCoords && options.trackCoords.length > 0) {
            const draggedCoord = [
              e.target.getLatLng().lng,
              e.target.getLatLng().lat,
            ];
            const snapped = snapToNearestTrackSegment(
              options.trackCoords,
              draggedCoord,
              map,
              50, // max pixel distance for snapping
            );

            // Update marker position to snapped location
            const snappedLatLng = L.latLng(snapped.coord[1], snapped.coord[0]);
            marker.setLatLng(snappedLatLng);

            // Update the callback with snapped coordinate and snapping info
            if (options.onDragEnd) {
              options.onDragEnd(index, snappedLatLng, {
                anchorSegmentId: snapped.segmentId,
                km: snapped.km,
              });
            }

            // Update the checkpoint's anchorSegmentId if controls array is mutable
            // This will trigger reactive updates to refresh the dashed lines
            if (controls[index]) {
              controls[index].anchorSegmentId = snapped.segmentId;
              controls[index].km = snapped.km;
            }
          } else {
            // No track snapping, just use the dragged position
            if (options.onDragEnd) {
              options.onDragEnd(index, e.target.getLatLng());
            }
          }
        });

        checkpointMarkers.push(marker);
      });

      // Draw dashed lines between checkpoints if either is not snapped to track
      for (let i = 0; i < controls.length - 1; i++) {
        const cp1 = controls[i];
        const cp2 = controls[i + 1];

        // Show dashed line if either checkpoint is not snapped to a track segment
        if (cp1.anchorSegmentId == null || cp2.anchorSegmentId == null) {
          const latlngs = [
            [cp1.coord.lat, cp1.coord.lng],
            [cp2.coord.lat, cp2.coord.lng],
          ];

          const line = L.polyline(latlngs, {
            color: "#666",
            weight: 2,
            opacity: 0.7,
            dashArray: "5, 10",
            interactive: false,
          }).addTo(map);

          checkpointLines.push(line);
        }
      }
    },

    /**
     * Clear all checkpoint markers.
     */
    clearControls() {
      checkpointMarkers.forEach((marker) => map.removeLayer(marker));
      checkpointMarkers = [];
      checkpointLines.forEach((line) => map.removeLayer(line));
      checkpointLines = [];
    },

    /**
     * Add click handler to map (for adding checkpoints in setup mode).
     * @param {Function} callback - Called with [lng, lat]
     */
    onMapClick(callback) {
      map.on("click", (e) => {
        callback([e.latlng.lng, e.latlng.lat]);
      });
    },

    /**
     * Remove all click handlers.
     */
    offMapClick() {
      map.off("click");
    },

    /**
     * Fit map to show all current layers.
     */
    fitToContent() {
      const bounds = L.latLngBounds();
      let hasContent = false;

      if (trackLayer) {
        bounds.extend(trackLayer.getBounds());
        hasContent = true;
      }

      checkpointMarkers.forEach((marker) => {
        bounds.extend(marker.getLatLng());
        hasContent = true;
      });

      if (hasContent) {
        map.fitBounds(bounds, { padding: [50, 50] });
      }
    },

    /**
     * Resize map (call after container size changes).
     */
    resize() {
      map.invalidateSize();
    },

    /**
     * Destroy the map instance.
     */
    destroy() {
      map.remove();
    },
  };

  return mapController;
}
