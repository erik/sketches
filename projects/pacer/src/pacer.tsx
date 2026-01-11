import { type LineString, type Position } from "geojson";

import { Livewire } from "./livewire.js";
import { GlobalStoreProps } from "./main.jsx";
import { type EventConfig } from "./shared/index.js";

const DEMO_DATA: EventConfig = {
  name: "2026 Two Volcano Sprint",
  startTime: new Date("2026-04-26T05:00Z"),
  endTime: new Date("2026-04-30T18:00Z"),
  routeLength: 1250,
  segments: [
    {
      id: "r1",
      title: "2vs full route",
      fileName: "2vs_combined.gpx",
      segmentLength: 1250,
      geometry: {
        type: "LineString",
        coordinates: [
          [14.382801, 40.820178],
          [15.026495, 37.614285],
          [14.026495, 36.614285],
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
    },
    {
      id: "m1",
      kind: "marker",
      name: "Maratea - halfway",
      segmentId: "r1",
      routeDistance: 650,
      coordinate: [15.026495, 37.614285],
      goalTime: new Date("2026-04-28T18:00Z"),
    },
    {
      id: "m2",
      kind: "control",
      name: "Ferry",
      note: "24h",
      segmentId: "r1",
      routeDistance: 950,
      coordinate: [15.026495, 36.614285],
      cutoffTime: new Date("2026-04-28T18:00Z"),
    },
    {
      id: "m3",
      kind: "finish",
      name: "Finish Line",
      note: "Town of Nicolosi after Etna descent",
      segmentId: "r1",
      coordinate: [0, 0],
    },
  ],
};

type ProgressEvent = {
  markerId: string;
  arrivalTime: Date;
  segmentPace: number;
};

type StoreProps = {
  state: "unstarted" | "inprogress" | "finished";
  event: EventConfig;
  progress: ProgressEvent[];
  userLocation?: Position;
  userLocationPermission: "unrequested" | "denied" | "granted";
};

type ComputedProps = {
  $currentDistance: number;
  $currentPace: number;
  $requiredPace: number;
};

const createStore = (g: Livewire<GlobalStoreProps>) => {
  const store = new Livewire<StoreProps, ComputedProps>(
    {
      state: "inprogress",
      event: DEMO_DATA,
      userLocationPermission: "unrequested",
      progress: [
        // TODO: populate some dummy data
      ],
    },
    g,
  );

  store.compute("$currentDistance", ({ userLocation }) => /* TODO */ 0);
  store.compute("$currentPace", ({ userLocation }) => /* TODO */ 0);
  store.compute("$requiredPace", ({ userLocation }) => /* TODO */ 0);

  return store;
};

export function createApp(globalStore: Livewire<GlobalStoreProps>) {
  const store = createStore(globalStore);

  // TODO: we want to build a ui that's something like this
  //
  //   CONTENT CONTENT CONTENT
  //   CONTENT CONTENT CONTENT
  //   CONTENT CONTENT CONTENT
  //   [TAB 1] [TAB 2] [TAB 3]
  //
  // this is to be viewed on mobile phones and should fit comfortably there.
  // limit scrolling. everything important available at a glance.
  //
  // Use daisyUI + the custom livewire framework in ./livewire.tsx
  //
  //
  // Tab1: stats
  //
  //   [ next control: ... ]
  //
  //   [ stat ] [ stat ] [ stat ]
  //   [ stat ] [ stat ] [ stat ]
  //
  // stats should include, distance, current pace, required pace to arrive at
  // next control (or finish) by cutoff time, what the next control point is, etc.
  // arrival ETA, color code green/red depending on feasibility of hitting cutoff
  //
  // Tab2: map
  //
  //   MAP VIEW
  //
  // map view should display route, markers, user location
  //
  // Tab3: check ins
  //
  //   [ btn: check in at next control (displayed if user location is close) ]
  //
  //   cp0: distance / pace (required or actual) / arrival time (allow edit)
  //   cp1: distance / pace (required or actual) / arrival time (allow edit)
  //   finish: distance / pace (required or actual) / arrival time (allow edit)
  return <main class="mx-auto">{/* TODO! */}</main>;
}
