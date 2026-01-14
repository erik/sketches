import { lineString, point } from "@turf/helpers";
import simplify from "@turf/simplify";
import nearestPointOnLine from "@turf/nearest-point-on-line";
import length from "@turf/length";
import lineSlice from "@turf/line-slice";

// Export commonly used Turf functions for direct use in other modules
export { nearestPointOnLine, length, lineSlice };

/**
 * Parse a GPX file and extract track coordinates.
 * @param {string} gpxText - Raw GPX XML text
 * @returns {[number, number][]} Array of [lng, lat] coordinates
 */
export function parseGPX(gpxText) {
  const parser = new DOMParser();
  const doc = parser.parseFromString(gpxText, "application/xml");

  const coords = [];

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
 * Calculate route position including distance along track and distance from track.
 * @param {[number, number][]} routeCoordinates - Route coordinates
 * @param {[number, number]} userLocation - User location [lng, lat]
 * @returns {{distanceFromStart: number, distanceFromTrack: number}}
 */
export function calculateRoutePosition(routeCoordinates, userLocation) {
  const line = lineString(routeCoordinates);
  const nearest = nearestPointOnLine(line, userLocation, {
    units: "kilometers",
  });

  // Calculate distance along track
  const start = point(routeCoordinates[0]);
  const snappedPoint = point(nearest.geometry.coordinates);
  const sliced = lineSlice(start, snappedPoint, line);
  const distanceFromStart = length(sliced, { units: "kilometers" });

  return {
    distanceFromStart,
    distanceFromTrack: nearest.properties.dist,
  };
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

  tracks.forEach((segment, segmentIndex) => {
    let track = segment.coords;
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
