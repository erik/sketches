import { EventConfig } from "./shared/index.js";

const startTime = Temporal.Now.instant().subtract({ hours: 36 });
const endTime = startTime.add({ hours: 128 });

export const DEMO_DATA: EventConfig = {
  name: "2026 Two Volcano Sprint",
  startTime,
  endTime,
  routeLength: 1250,
  segments: [
    {
      id: "r1",
      title: "2vs full route",
      fileName: "2vs_combined.gpx",
      segmentLength: 1250,
      geometry: {
        type: "LineString",
        coordinates: [],
      },
    },
  ],
  markers: [
    {
      id: "m0",
      kind: "start",
      name: "Start Point",
      note: "Under the gaze of the bifacial head",
      segmentId: "r1",
      coordinate: [14.382801, 40.820178],
      routeDistance: 0,
    },
    {
      id: "m1",
      kind: "marker",
      name: "Maratea - halfway",
      segmentId: "r1",
      routeDistance: 650,
      coordinate: [16.026495, 39.614285],
      goalTime: startTime.add({ hours: 32 }),
    },
    {
      id: "m2",
      kind: "control",
      name: "Ferry",
      note: "24h",
      segmentId: "r1",
      routeDistance: 950,
      coordinate: [15.626495, 38.214285],
      cutoffTime: startTime.add({ hours: 48 }),
      goalTime: startTime.add({ hours: 40 }),
    },
    {
      id: "m3",
      kind: "finish",
      name: "Finish Line",
      note: "Town of Nicolosi after Etna descent",
      segmentId: "r1",
      coordinate: [15.026495, 37.614285],
      routeDistance: 1250,
      cutoffTime: endTime,
    },
  ],
};
