import { EventConfig, Meters } from "./shared/index.js";

const startTime = Temporal.Now.instant().subtract({ hours: 36 });
const endTime = startTime.add({ hours: 128 });

export const DEMO_DATA: EventConfig = {
  name: "2026 Two Volcano Sprint",
  startTime,
  endTime,
  routeLength: Meters(1250000),
  segments: [
    {
      id: "r1",
      title: "2vs full route",
      fileName: "2vs_combined.gpx",
      segmentLength: Meters(1250000),
      geometry: {
        type: "LineString",
        coordinates: [
          [40.820178, 14.382801], // Naples area (start)
          [40.214285, 14.856495], // South towards Salerno
          [39.814285, 15.426495], // Calabria
          [39.614285, 16.026495], // Maratea area
          [38.914285, 15.926495], // Further south
          [38.214285, 15.626495], // Ferry crossing area
          [37.814285, 15.326495], // Sicily approach
          [37.614285, 15.026495], // Nicolosi/Etna area (finish)
        ],
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
      routeDistance: Meters(0),
    },
    {
      id: "m1",
      kind: "marker",
      name: "Maratea - halfway",
      segmentId: "r1",
      routeDistance: Meters(650000),
      coordinate: [16.026495, 39.614285],
      goalTime: startTime.add({ hours: 32 }),
    },
    {
      id: "m2",
      kind: "control",
      name: "Ferry",
      note: "24h",
      segmentId: "r1",
      routeDistance: Meters(950000),
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
      routeDistance: Meters(1250000),
      cutoffTime: endTime,
    },
  ],
};
