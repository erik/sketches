import { lineString, point } from "@turf/helpers";
import nearestPointOnLine from "@turf/nearest-point-on-line";
import length from "@turf/length";
import lineSlice from "@turf/line-slice";
import L from "leaflet";
import { Segment } from "./index.js";

// Export commonly used Turf functions for direct use in other modules
export { nearestPointOnLine, length, lineSlice };

/**
 * Calculate route position including distance along track and distance from track.
 */
export function calculateRoutePosition(
  routeCoordinates: [number, number][],
  userLocation: [number, number],
): { distanceFromStart: number; distanceFromTrack: number } {
  const line = lineString(routeCoordinates);
  const nearest = nearestPointOnLine(line, userLocation, { units: "meters" });

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
 */
export function calculateTrackLength(coords: [number, number][]): number {
  if (coords.length < 2) return 0;
  const line = lineString(coords);
  return length(line, { units: "kilometers" });
}

/**
 * Find the nearest track segment from multiple tracks and snap to it.
 */
export function snapToNearestTrackSegment(
  tracks: Segment[],
  clickLocation: [number, number],
  map: L.Map,
  maxPixelDistance: number = 200,
): {
  coord: [number, number];
  km: number;
  segmentId: string | null;
  pixelDistance: number;
} {
  let nearestResult: {
    coord: [number, number];
    km: number;
    segmentId: string | null;
    pixelDistance: number;
  } | null = null;
  let minPixelDistance = Infinity;

  tracks.forEach((segment, segmentIndex) => {
    let track = segment.geometry;
    if (track.coordinates.length < 2) return;

    const pt = point(clickLocation);
    const snapped = nearestPointOnLine(track, pt, { units: "kilometers" });

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
      const km = snapped.properties.dist;
      nearestResult = {
        coord: snapped.geometry.coordinates as [number, number],
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
