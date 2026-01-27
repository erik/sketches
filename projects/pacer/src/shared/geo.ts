import { lineString, point } from "@turf/helpers";
import nearestPointOnLine from "@turf/nearest-point-on-line";
import length from "@turf/length";
import lineSlice from "@turf/line-slice";
import L from "leaflet";
import { Segment, Meters } from "./index.js";

export function calculateRoutePosition(
  routeCoordinates: [number, number][],
  userLocation: [number, number],
): { distanceFromStart: Meters; distanceFromTrack: number } {
  const line = lineString(routeCoordinates);
  const nearest = nearestPointOnLine(line, userLocation, { units: "meters" });

  const start = point(routeCoordinates[0]);
  const snappedPoint = point(nearest.geometry.coordinates);
  const sliced = lineSlice(start, snappedPoint, line);
  const distanceFromStart = Meters(length(sliced, { units: "meters" }));

  return {
    distanceFromStart,
    distanceFromTrack: nearest.properties.dist,
  };
}

export function calculateTrackLength(coords: [number, number][]): number {
  if (coords.length < 2) return 0;
  const line = lineString(coords);
  return length(line, { units: "kilometers" });
}

export function snapToNearestTrackSegment(
  tracks: Segment[],
  clickLocation: [number, number],
  map: L.Map,
  maxPixelDistance: number = 200,
): {
  coord: [number, number];
  meters: Meters;
  segmentId: string | null;
  pixelDistance: number;
} {
  let nearestResult: {
    coord: [number, number];
    meters: Meters;
    segmentId: string | null;
    pixelDistance: number;
  } | null = null;
  let minPixelDistance = Infinity;

  tracks.forEach((segment, segmentIndex) => {
    let track = segment.geometry;
    if (track.coordinates.length < 2) return;

    const pt = point(clickLocation);
    const snapped = nearestPointOnLine(track, pt, { units: "kilometers" });

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

      const snappedPoint = point(snapped.geometry.coordinates);
      const slicedLine = lineSlice(
        point(track.coordinates[0]),
        snappedPoint,
        track,
      );
      const distanceAlongTrack = length(slicedLine, { units: "meters" });

      nearestResult = {
        coord: snapped.geometry.coordinates as [number, number],
        meters: Meters(distanceAlongTrack),
        segmentId: segmentIndex.toString(),
        pixelDistance: pixelDistance,
      };
    }
  });

  if (!nearestResult) {
    return {
      coord: clickLocation,
      meters: Meters(0),
      segmentId: null,
      pixelDistance: Infinity,
    };
  }

  return nearestResult;
}
