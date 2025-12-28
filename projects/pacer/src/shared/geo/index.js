/**
 * Geo Module - Pure geospatial operations
 * No dependencies on UI, state, or other modules
 * Uses turf.js for geospatial calculations
 */

import { lineString, point } from "@turf/helpers";
import simplify from "@turf/simplify";
import nearestPointOnLine from "@turf/nearest-point-on-line";
import length from "@turf/length";
import lineSlice from "@turf/line-slice";
import distance from "@turf/distance";
import along from "@turf/along";
import bbox from "@turf/bbox";

/**
 * Parse GPX file and extract coordinates
 * @param {string} gpxText - GPX XML content
 * @returns {[number, number][]} Array of [lng, lat] coordinates
 */
export function parseGPX(gpxText) {
  const parser = new DOMParser();
  const doc = parser.parseFromString(gpxText, "application/xml");

  const coords = [];

  // Try track points first
  const trkpts = doc.querySelectorAll("trkpt");
  if (trkpts.length > 0) {
    trkpts.forEach((pt) => {
      const lat = parseFloat(pt.getAttribute("lat"));
      const lon = parseFloat(pt.getAttribute("lon"));
      if (!isNaN(lat) && !isNaN(lon)) {
        coords.push([lon, lat]);
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
 * Simplify track using Douglas-Peucker algorithm
 * @param {[number, number][]} coords - Track coordinates
 * @param {number} tolerance - Simplification tolerance (default: 0.001)
 * @returns {[number, number][]} Simplified coordinates
 */
export function simplifyTrack(coords, tolerance = 0.001) {
  if (coords.length < 2) return coords;
  const line = lineString(coords);
  const simplified = simplify(line, { tolerance, highQuality: true });
  return simplified.geometry.coordinates;
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
 * Snap point to nearest location on track
 * @param {[number, number][]} track - Track coordinates
 * @param {[number, number]} location - Point to snap
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
 * Calculate distance along track to target point
 * @param {[number, number][]} track - Track coordinates
 * @param {[number, number]} targetPoint - Target point
 * @returns {number} Distance in km
 */
export function calculateDistanceAlongTrack(track, targetPoint) {
  if (track.length < 2) return 0;

  const line = lineString(track);
  const target = point(targetPoint);
  const snapped = nearestPointOnLine(line, target);

  const start = point(track[0]);
  const sliced = lineSlice(start, snapped, line);

  return length(sliced, { units: "kilometers" });
}

/**
 * Find checkpoint position on track
 * @param {[number, number][]} track - Track coordinates
 * @param {[number, number]} clickLocation - Click location
 * @returns {{coord: [number, number], km: number}}
 */
export function findCheckpointOnTrack(track, clickLocation) {
  const snapped = snapToTrack(track, clickLocation);
  if (!snapped) {
    return { coord: clickLocation, km: 0 };
  }
  return { coord: snapped.coord, km: snapped.km };
}

/**
 * Validate checkpoint km value
 * @param {[number, number][]} track - Track coordinates
 * @param {number} km - Distance to validate
 * @returns {boolean} True if valid
 */
export function isValidCheckpointKm(track, km) {
  const totalLength = calculateTrackLength(track);
  return km >= 0 && km <= totalLength;
}

/**
 * Get coordinate at specific km along track
 * @param {[number, number][]} track - Track coordinates
 * @param {number} targetKm - Target distance
 * @returns {[number, number] | null}
 */
export function getCoordAtKm(track, targetKm) {
  if (track.length < 2) return null;

  const line = lineString(track);
  const totalLength = length(line, { units: "kilometers" });

  if (targetKm < 0 || targetKm > totalLength) return null;
  if (targetKm === 0) return track[0];
  if (targetKm >= totalLength) return track[track.length - 1];

  const pt = along(line, targetKm, { units: "kilometers" });
  return pt.geometry.coordinates;
}

/**
 * Calculate track bounds for map fitting
 * @param {[number, number][]} coords - Track coordinates
 * @returns {[[number, number], [number, number]] | null} Bounding box
 */
export function getTrackBounds(coords) {
  if (coords.length === 0) return null;

  const line = lineString(coords);
  const box = bbox(line);
  return [
    [box[0], box[1]],
    [box[2], box[3]],
  ];
}

/**
 * Calculate distance between two coordinates
 * @param {[number, number]} coord1 - First coordinate
 * @param {[number, number]} coord2 - Second coordinate
 * @returns {number} Distance in km
 */
export function calculateDistance(coord1, coord2) {
  const pt1 = point(coord1);
  const pt2 = point(coord2);
  return distance(pt1, pt2, { units: "kilometers" });
}

/**
 * Calculate distance between checkpoints along track
 * @param {Object} cpA - First checkpoint
 * @param {Object} cpB - Second checkpoint
 * @returns {number} Distance in km
 */
export function distanceBetweenCheckpoints(cpA, cpB) {
  return Math.abs(cpB.km - cpA.km);
}

/**
 * Sort checkpoints by distance along track
 * @param {Array} checkpoints - Checkpoints to sort
 * @returns {Array} Sorted checkpoints
 */
export function sortCheckpointsByDistance(checkpoints) {
  return [...checkpoints].sort((a, b) => a.km - b.km);
}
