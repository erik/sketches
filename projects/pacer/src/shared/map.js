/**
 * Map visualization for tracks and checkpoints using Leaflet.
 */

import L from "leaflet";
import "leaflet/dist/leaflet.css";

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
  let userMarker = null;
  let snappedMarker = null;
  let lineToTrackLayer = null;

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
      if (options.fitBounds !== false) {
        map.fitBounds(trackLayer.getBounds(), { padding: [50, 50] });
      }
    },

    /**
     * Clear the track from the map.
     */
    clearTrack() {
      if (trackLayer) {
        map.removeLayer(trackLayer);
        trackLayer = null;
      }
    },

    /**
     * Display checkpoints on the map.
     * @param {Array} checkpoints - Checkpoint objects with {id, name, coord, km}
     * @param {Object} options - Display options
     */
    showCheckpoints(checkpoints, options = {}) {
      // Clear existing markers
      this.clearCheckpoints();

      checkpoints.forEach((cp, index) => {
        const { lng, lat } = cp.coord;

        const name = cp.name || `CP ${index + 1}`;

        // Create custom icon for start/finish
        let icon = L.divIcon({
          className: "",
          html: `<div class="w-3 h-3 -translate-1/2 rounded-full bg-primary tooltip hover:w-6 hover:h-6 transition-all" data-tip="${name}"></div>`,
          iconSize: [12, 12],
          iconAnchor: [0, 0],
        });

        const marker = L.marker([lat, lng], { icon })
          .addTo(map)
          .bindPopup(
            `<b>${name}</b><br>${cp.km.toFixed(1)} km${cp.cutoff ? `<br>Cutoff: ${new Date(cp.cutoff).toLocaleString()}` : ""}`,
          );

        // Click handler for setup mode
        if (options.onClick) {
          marker.on("click", () => options.onClick(cp));
        }

        // Draggable in setup mode
        marker.dragging.enable();
        marker.on("dragend", (e) => {
          const { lng, lat } = e.target.getLatLng();
          if (options.onDragEnd) {
            options.onDragEnd(index, { lng, lat });
          }
        });

        checkpointMarkers.push(marker);
      });

      // Draw dashed lines between checkpoints where anchorSegmentId is null
      for (let i = 0; i < checkpoints.length - 1; i++) {
        const cp1 = checkpoints[i];
        const cp2 = checkpoints[i + 1];

        if (cp1.anchorSegmentId == null) {
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
    clearCheckpoints() {
      checkpointMarkers.forEach((marker) => map.removeLayer(marker));
      checkpointMarkers = [];
      checkpointLines.forEach((line) => map.removeLayer(line));
      checkpointLines = [];
    },

    /**
     * Show user's current location.
     * @param {[number, number]} coord - [lng, lat]
     */
    showUserLocation(coord) {
      const [lng, lat] = coord;

      if (userMarker) {
        userMarker.setLatLng([lat, lng]);
      } else {
        userMarker = L.circleMarker([lat, lng], {
          radius: 8,
          fillColor: "#3b82f6",
          color: "#ffffff",
          weight: 2,
          opacity: 1,
          fillOpacity: 1,
        })
          .addTo(map)
          .bindPopup("Your Location");
      }

      map.setView([lat, lng], Math.max(map.getZoom(), 12));
    },

    /**
     * Show snapped location on track.
     * @param {[number, number]} coord - [lng, lat]
     * @param {number} km - Distance along track
     */
    showSnappedLocation(coord, km) {
      const [lng, lat] = coord;

      if (snappedMarker) {
        snappedMarker.setLatLng([lat, lng]);
        snappedMarker
          .getPopup()
          .setContent(`<b>On Track</b><br>${km.toFixed(1)} km`);
      } else {
        snappedMarker = L.circleMarker([lat, lng], {
          radius: 6,
          fillColor: "#10b981",
          color: "#ffffff",
          weight: 2,
          opacity: 1,
          fillOpacity: 1,
        })
          .addTo(map)
          .bindPopup(`<b>On Track</b><br>${km.toFixed(1)} km`);
      }
    },

    /**
     * Clear user and snapped location markers.
     */
    clearLocationMarkers() {
      if (userMarker) {
        map.removeLayer(userMarker);
        userMarker = null;
      }
      if (snappedMarker) {
        map.removeLayer(snappedMarker);
        snappedMarker = null;
      }
      this.clearLineToTrack();
    },

    /**
     * Draw a dashed line from user location to track.
     * @param {[number, number]} userCoord - User's [lng, lat]
     * @param {[number, number]} trackCoord - Nearest point on track [lng, lat]
     */
    drawLineToTrack(userCoord, trackCoord) {
      this.clearLineToTrack();

      const latLngs = [
        [userCoord[1], userCoord[0]],
        [trackCoord[1], trackCoord[0]],
      ];

      lineToTrackLayer = L.polyline(latLngs, {
        color: "#6b7280",
        weight: 2,
        opacity: 0.6,
        dashArray: "5, 10",
      }).addTo(map);
    },

    /**
     * Clear the line to track.
     */
    clearLineToTrack() {
      if (lineToTrackLayer) {
        map.removeLayer(lineToTrackLayer);
        lineToTrackLayer = null;
      }
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
