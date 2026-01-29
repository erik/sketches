import { type Point, type Feature, type LineString } from "geojson";
import { gpx as parseGpxXml } from "@tmcw/togeojson";
import { lineString, point } from "@turf/helpers";

type GpxTrackProps = { name?: string };
type GpxMarkerProps = { name?: string; note?: string; icon?: string };

type GpxFile = {
  tracks: Feature<LineString, GpxTrackProps>[];
  markers: Feature<Point, GpxMarkerProps>[];
};

export function parseGPX(gpxText: string): GpxFile {
  const parser = new DOMParser();
  const doc = parser.parseFromString(gpxText, "application/xml");
  const gpx = parseGpxXml(doc);

  const tracks: Feature<LineString, GpxTrackProps>[] = [];
  const markers: Feature<Point, GpxMarkerProps>[] = [];

  for (const ft of gpx.features) {
    console.log(ft.geometry.type, ft.properties);
    switch (ft.geometry.type) {
      case "LineString":
        tracks.push(ft as Feature<LineString, GpxTrackProps>);
        break;

      case "MultiLineString":
        for (const [idx, line] of ft.geometry.coordinates.entries()) {
          tracks.push(
            lineString(line, { name: `${ft.properties.name} (${idx + 1})` }),
          );
        }
        break;

      case "Point":
        markers.push(
          point(ft.geometry.coordinates, {
            name: ft.properties?.name,
            note: ft.properties?.desc || ft.properties?.cmt,
            icon: ft.properties?.sym,
          }),
        );
        break;
    }
  }

  return {
    tracks,
    markers,
  };
}
