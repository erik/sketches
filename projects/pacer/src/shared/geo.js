/**
 * Geospatial operations: GPX parsing, track simplification, distance calculations.
 * Uses turf.js for heavy lifting.
 */

// We'll import turf modules. With Vite, you can npm install these:
// npm install @turf/helpers @turf/simplify @turf/nearest-point-on-line @turf/length @turf/line-slice @turf/distance @turf/along @turf/bbox

import { lineString, point } from "@turf/helpers";
import simplify from "@turf/simplify";
import nearestPointOnLine from "@turf/nearest-point-on-line";
import length from "@turf/length";
import lineSlice from "@turf/line-slice";
import distance from "@turf/distance";
import along from "@turf/along";
import bbox from "@turf/bbox";

/**
 * Parse a GPX file and extract track coordinates.
 * @param {string} gpxText - Raw GPX XML text
 * @returns {[number, number][]} Array of [lng, lat] coordinates
 */
export function parseGPX(gpxText) {
  const parser = new DOMParser();
  const doc = parser.parseFromString(gpxText, "application/xml");

  const coords = [];

  // Try track points first (most common)
  const trkpts = doc.querySelectorAll("trkpt");
  if (trkpts.length > 0) {
    trkpts.forEach((pt) => {
      const lat = parseFloat(pt.getAttribute("lat"));
      const lon = parseFloat(pt.getAttribute("lon"));
      if (!isNaN(lat) && !isNaN(lon)) {
        coords.push([lon, lat]); // GeoJSON is [lng, lat]
      }
    });
    return coords;
  }

  // Fall back to route points
  const rtepts = doc.querySelectorAll("rtept");
  rtepts.forEach((pt) => {
    const lat = parseFloat(pt.getAttribute("lat"));
    const lon = parseFloat(pt.getAttribute("lon"));
    if (!isNaN(lat) && !isNaN(lon)) {
      coords.push([lon, lat]);
    }
  });

  return coords;
}

/**
 * Simplify a track using Douglas-Peucker algorithm.
 * @param {[number, number][]} coords - Array of [lng, lat]
 * @param {number} tolerance - Simplification tolerance in degrees (0.001 ≈ 100m)
 * @returns {[number, number][]}
 */
export function simplifyTrack(coords, tolerance = 0.001) {
  if (coords.length < 2) return coords;

  const line = lineString(coords);
  const simplified = simplify(line, { tolerance, highQuality: true });

  return simplified.geometry.coordinates;
}

/**
 * Calculate the total length of a track in kilometers.
 * @param {[number, number][]} coords
 * @returns {number}
 */
export function trackLength(coords) {
  if (coords.length < 2) return 0;
  const line = lineString(coords);
  return length(line, { units: "kilometers" });
}

/**
 * Find the nearest point on the track to a given location.
 * @param {[number, number][]} track - Track coordinates
 * @param {[number, number]} location - [lng, lat] to snap
 * @returns {{coord: [number, number], km: number, index: number} | null}
 */
export function snapToTrack(track, location) {
  if (track.length < 2) return null;

  const line = lineString(track);
  const pt = point(location);
  const snapped = nearestPointOnLine(line, pt, { units: "kilometers" });

  // Calculate distance along track
  const km = calculateDistanceAlongTrack(track, snapped.geometry.coordinates);

  return {
    coord: snapped.geometry.coordinates,
    km,
    index: snapped.properties.index,
  };
}

/**
 * Calculate distance along track to a specific point.
 * @param {[number, number][]} track
 * @param {[number, number]} targetPoint
 * @returns {number} Distance in km
 */
export function calculateDistanceAlongTrack(track, targetPoint) {
  if (track.length < 2) return 0;

  const line = lineString(track);
  const target = point(targetPoint);
  const snapped = nearestPointOnLine(line, target);

  // Get the segment before the snapped point
  const start = point(track[0]);
  const sliced = lineSlice(start, snapped, line);

  return length(sliced, { units: "kilometers" });
}

/**
 * Find checkpoint coordinates by snapping to nearest point on track.
 * Used when placing checkpoints by clicking on map.
 * @param {[number, number][]} track
 * @param {[number, number]} clickLocation
 * @returns {{coord: [number, number], km: number}}
 */
export function findCheckpointOnTrack(track, clickLocation) {
  const snapped = snapToTrack(track, clickLocation);
  if (!snapped) {
    // Fallback if track is empty
    return { coord: clickLocation, km: 0 };
  }
  return { coord: snapped.coord, km: snapped.km };
}

/**
 * Calculate track length in kilometers
 * @param {[number, number][]} coords - Track coordinates
 * @returns {number} Length in km
 */
export function calculateTrackLength(coords) {
  if (coords.length < 2) return 0;
  const line = lineString(coords);
  return length(line, { units: "kilometers" });
}

/**
 * Find the nearest track segment from multiple tracks and snap to it.
 * @param {[number, number][][]} tracks - Array of track segments
 * @param {[number, number]} clickLocation - [lng, lat]
 * @param {L.Map} map - Leaflet map instance for pixel distance calculation
 * @param {number} maxPixelDistance - Maximum pixel distance for snapping
 * @returns {{coord: [number, number], km: number, segmentId: string|null}}
 */
export function snapToNearestTrackSegment(
  tracks,
  clickLocation,
  map,
  maxPixelDistance = 200,
) {
  let nearestResult = null;
  let minPixelDistance = Infinity;

  tracks.forEach((track, segmentIndex) => {
    if (track.length < 2) return;

    const line = lineString(track);
    const pt = point(clickLocation);
    const snapped = nearestPointOnLine(line, pt, { units: "kilometers" });

    // Calculate pixel distance between click and snapped point
    const clickLatLng = L.latLng(clickLocation[1], clickLocation[0]);
    const snappedLatLng = L.latLng(
      snapped.geometry.coordinates[1],
      snapped.geometry.coordinates[0],
    );
    const pixelDistance = map
      .latLngToLayerPoint(clickLatLng)
      .distanceTo(map.latLngToLayerPoint(snappedLatLng));

    if (pixelDistance <= maxPixelDistance && pixelDistance < minPixelDistance) {
      minPixelDistance = pixelDistance;

      // Calculate distance along track
      const km = calculateDistanceAlongTrack(
        track,
        snapped.geometry.coordinates,
      );

      nearestResult = {
        coord: snapped.geometry.coordinates,
        km: km,
        segmentId: segmentIndex.toString(),
        pixelDistance: pixelDistance,
      };
    }
  });

  // If no suitable segment found within distance, return original location
  if (!nearestResult) {
    return {
      coord: clickLocation,
      km: 0,
      segmentId: null,
      pixelDistance: Infinity,
    };
  }

  return nearestResult;
}

/**
 * Validate that a checkpoint km value is reasonable for the track.
 * @param {[number, number][]} track
 * @param {number} km
 * @returns {boolean}
 */
export function isValidCheckpointKm(track, km) {
  const totalLength = trackLength(track);
  return km >= 0 && km <= totalLength;
}

/**
 * Get the coordinate at a specific km along the track.
 * Used when user manually enters a km value for a checkpoint.
 * @param {[number, number][]} track
 * @param {number} targetKm
 * @returns {[number, number] | null}
 */
export function getCoordAtKm(track, targetKm) {
  if (track.length < 2) return null;

  const line = lineString(track);
  const totalLength = length(line, { units: "kilometers" });

  if (targetKm < 0 || targetKm > totalLength) return null;
  if (targetKm === 0) return track[0];
  if (targetKm >= totalLength) return track[track.length - 1];

  // Use turf along to get point at distance
  const pt = along(line, targetKm, { units: "kilometers" });
  return pt.geometry.coordinates;
}

/**
 * Calculate bounds for a track (for map centering).
 * @param {[number, number][]} coords
 * @returns {[[number, number], [number, number]] | null} [[minLng, minLat], [maxLng, maxLat]]
 */
export function getTrackBounds(coords) {
  if (coords.length === 0) return null;

  const line = lineString(coords);
  const box = bbox(line);

  // bbox returns [minLng, minLat, maxLng, maxLat]
  // Convert to [[minLng, minLat], [maxLng, maxLat]]
  return [
    [box[0], box[1]],
    [box[2], box[3]],
  ];
}

/**
 * Calculate distance between two coordinates.
 * @param {[number, number]} coord1 - [lng, lat]
 * @param {[number, number]} coord2 - [lng, lat]
 * @returns {number} Distance in km
 */
export function calculateDistance(coord1, coord2) {
  const pt1 = point(coord1);
  const pt2 = point(coord2);
  return distance(pt1, pt2, { units: "kilometers" });
}

/**
 * Calculate distance between two checkpoints along the track.
 * @param {import('./state.js').Checkpoint} cpA
 * @param {import('./state.js').Checkpoint} cpB
 * @returns {number} Distance in km
 */
export function distanceBetweenCheckpoints(cpA, cpB) {
  return Math.abs(cpB.km - cpA.km);
}

/**
 * Sort checkpoints by distance along track.
 * @param {import('./state.js').Checkpoint[]} checkpoints
 * @returns {import('./state.js').Checkpoint[]}
 */
export function sortCheckpointsByDistance(checkpoints) {
  return [...checkpoints].sort((a, b) => a.km - b.km);
}
