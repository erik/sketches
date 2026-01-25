import L, { LatLngTuple } from "leaflet";
import "leaflet/dist/leaflet.css";

import { createMap } from "../shared/map.js";
import { RouteMarker, Segment } from "../shared/index.js";

import { formatDateTimeCompact } from "../pacer.jsx";
import { snapToNearestTrackSegment } from "../shared/geo.js";
import { parseGPX } from "../shared/gpx.js";
import { htmlTemplate, Livewire, type Children } from "../livewire.js";
import { GlobalStoreProps } from "../main.jsx";
import length from "@turf/length";
import nearestPointOnLine from "@turf/nearest-point-on-line";
import lineSliceAlong from "@turf/line-slice-along";

type StoreProps = {
  trackName: string;
  startTime?: Temporal.Instant;
  endTime?: Temporal.Instant;
  segments: Segment[];
  markers: RouteMarker[];
};

type ComputedProps = {
  $valid: boolean;
};

// Demo data for testing - set to null to start with empty state
const DEMO_SETUP_DATA = (() => {
  const demoStartTime = Temporal.Now.instant().add({ hours: 2 });
  const demoEndTime = demoStartTime.add({ hours: 128 });

  return {
    trackName: "2026 Two Volcano Sprint",
    startTime: demoStartTime,
    endTime: demoEndTime,
    segments: [
      {
        id: "r1",
        title: "2vs full route",
        fileName: "2vs_combined.gpx",
        segmentLength: 1250000, // 1250km in meters
        geometry: {
          type: "LineString" as const,
          coordinates: [
            [14.382801, 40.820178], // Naples area (start)
            [14.856495, 40.214285], // South towards Salerno
            [15.426495, 39.814285], // Calabria
            [16.026495, 39.614285], // Maratea area
            [15.926495, 38.914285], // Further south
            [15.626495, 38.214285], // Ferry crossing area
            [15.326495, 37.814285], // Sicily approach
            [15.026495, 37.614285], // Nicolosi/Etna area (finish)
          ],
        },
      },
    ],
    markers: [
      {
        id: "m1",
        kind: "marker" as const,
        name: "Maratea - halfway",
        segmentId: "r1",
        coordinate: [16.026495, 39.614285] as [number, number],
        routeDistance: 650,
        goalTime: demoStartTime.add({ hours: 32 }),
      },
      {
        id: "m2",
        kind: "control" as const,
        name: "Ferry",
        note: "24h",
        segmentId: "r1",
        coordinate: [15.626495, 38.214285] as [number, number],
        routeDistance: 950,
        cutoffTime: demoStartTime.add({ hours: 48 }),
        goalTime: demoStartTime.add({ hours: 40 }),
      },
      {
        id: "m3",
        kind: "finish" as const,
        name: "Finish Line",
        note: "Town of Nicolosi after Etna descent",
        segmentId: "r1",
        coordinate: [15.026495, 37.614285] as [number, number],
        routeDistance: 1250,
        cutoffTime: demoEndTime,
      },
    ],
  };
})();

const SVG_CHECK = () => htmlTemplate`<svg
  xmlns="http://www.w3.org/2000/svg"
  class="h-6 w-6"
  fill="none"
  viewBox="0 0 24 24"
  stroke="currentColor"
 >
   <path
     stroke-linecap="round"
     stroke-linejoin="round"
     stroke-width="2"
     d="M5 13l4 4L19 7"
   />
</svg>`;

const createStore = (
  global: Livewire<GlobalStoreProps>,
): Livewire<StoreProps, ComputedProps> => {
  const store = new Livewire<StoreProps, ComputedProps>(
    DEMO_SETUP_DATA || {
      trackName: "Untitled",
      startTime: null,
      endTime: null,
      segments: [],
      markers: [],
    },
  ).compute(
    "$valid",
    ({ trackName, startTime, endTime }) =>
      trackName?.length && !!startTime && !!endTime,
  );

  return store;
};

const Fieldset = (props: { title: string }, ...children: Children) => {
  return (
    <fieldset class="fieldset bg-base-200 border border-base-300 p-2">
      <legend class="fieldset-legend">{props.title}</legend>
      {...children}
    </fieldset>
  );
};

const EditableText = ({
  onChange,
  value,
  placeholder,
}: {
  onChange: (value: string) => void;
  value: string;
  placeholder: string;
}) => {
  const store = new Livewire({
    editing: false,
    textValue: value,
  });

  return store.render("editing", ({ editing, textValue }) =>
    editing ? (
      <input
        type="text"
        value={textValue || ""}
        placeholder={placeholder}
        autoFocus
        onBlur={(e: Event) => {
          store.$.editing = false;
          store.$.textValue = (e.target as HTMLInputElement).value;
          store.$.textValue !== placeholder && onChange(store.$.textValue);
        }}
        class="input input-sm w-full"
      />
    ) : (
      <div
        onClick={() => (store.$.editing = true)}
        class="cursor-pointer font-medium hover:bg-base-200 px-2 py-1 rounded -ml-2"
      >
        {textValue || placeholder}
      </div>
    ),
  );
};

const SimpleRow = (_props: {}, ...children: Children) => {
  return (
    <li class="border border-base-300 bg-base-100 hover:bg-base-200 p-3">
      {...children}
    </li>
  );
};

const MarkerDropdown = ({
  store,
  index,
  marker,
}: {
  store: Livewire<StoreProps, ComputedProps>;
  index: number;
  marker: RouteMarker;
}) => {
  const dropdownState = new Livewire({
    showCutoffInput: false,
    showGoalInput: false,
    cutoffInputValue: "",
    goalInputValue: "",
  });

  const isTimed = !!(marker.goalTime || marker.cutoffTime);

  const clearTiming = (e: Event) => {
    e.preventDefault();

    store.$.markers[index] = {
      ...marker,
      goalTime: undefined,
      cutoffTime: undefined,
      kind: "marker",
    };
    store.$.markers = [...store.$.markers];
  };

  const setCutoffTime = (value: string) => {
    if (value) {
      const instant = Temporal.PlainDateTime.from(value)
        .toZonedDateTime(Temporal.Now.timeZoneId())
        .toInstant();
      store.$.markers[index].cutoffTime = instant;
      store.$.markers = [...store.$.markers];
    }
    dropdownState.$.showCutoffInput = false;
  };

  const setGoalTime = (value: string) => {
    if (value) {
      const instant = Temporal.PlainDateTime.from(value)
        .toZonedDateTime(Temporal.Now.timeZoneId())
        .toInstant();
      store.$.markers[index].goalTime = instant;
      store.$.markers = [...store.$.markers];
    }
    dropdownState.$.showGoalInput = false;
  };

  const removeMarker = () => {
    store.$.markers.splice(index, 1);
    store.$.markers = [...store.$.markers];
  };

  return (
    <div class="dropdown dropdown-end">
      <button class="btn btn-soft btn-sm" type="button" tabindex="0">
        ...
      </button>
      <ul class="dropdown-content menu bg-base-100 rounded-box shadow border border-base-300 min-w-64 p-2 z-999">
        {isTimed && (
          <li>
            <button onClick={clearTiming}>Clear timing</button>
          </li>
        )}

        <li>
          <button
            onClick={(e: Event) => {
              e.preventDefault();
              dropdownState.$.showCutoffInput =
                !dropdownState.$.showCutoffInput;
            }}
            class="flex justify-between items-center"
          >
            <span>Set cutoff time</span>
            {marker.cutoffTime && (
              <span class="badge badge-xs badge-soft">
                {formatDateTimeCompact(marker.cutoffTime)}
              </span>
            )}
          </button>
        </li>

        <dropdownState.reactiveIf key={"showCutoffInput"}>
          {() => (
            <div class="p-2 flex items-center gap-2">
              <input
                type="datetime-local"
                value={
                  marker.cutoffTime
                    ? marker.cutoffTime
                        .toZonedDateTimeISO(Temporal.Now.timeZoneId())
                        .toPlainDateTime()
                        .toString()
                        .slice(0, 16)
                    : ""
                }
                autoFocus
                class="input input-sm w-full"
                onInput={(e: Event) => {
                  dropdownState.$.cutoffInputValue = (
                    e.target as HTMLInputElement
                  ).value;
                }}
              />
              <button
                class="btn btn-sm btn-square"
                onClick={() => setCutoffTime(dropdownState.$.cutoffInputValue)}
              >
                {SVG_CHECK}
              </button>
            </div>
          )}
        </dropdownState.reactiveIf>

        <li>
          <button
            onClick={(e: Event) => {
              e.preventDefault();
              dropdownState.$.showGoalInput = !dropdownState.$.showGoalInput;
            }}
            class="flex justify-between items-center"
          >
            <span>Set goal time</span>
            {marker.goalTime && (
              <span class="badge badge-xs badge-soft">
                {formatDateTimeCompact(marker.goalTime)}
              </span>
            )}
          </button>
        </li>

        <dropdownState.reactiveIf key={"showGoalInput"}>
          {() => (
            <div class="p-2 flex items-center gap-2">
              <input
                type="datetime-local"
                value={
                  marker.goalTime
                    ? marker.goalTime
                        .toZonedDateTimeISO(Temporal.Now.timeZoneId())
                        .toPlainDateTime()
                        .toString()
                        .slice(0, 16)
                    : ""
                }
                autoFocus
                class="input input-sm w-full"
                onInput={(e: Event) => {
                  dropdownState.$.goalInputValue = (
                    e.target as HTMLInputElement
                  ).value;
                }}
              />
              <button
                class="btn btn-sm btn-square"
                onClick={() => setGoalTime(dropdownState.$.goalInputValue)}
              >
                {SVG_CHECK}
              </button>
            </div>
          )}
        </dropdownState.reactiveIf>

        <hr />

        <li>
          <a onClick={removeMarker} class="text-error">
            Remove marker
          </a>
        </li>
      </ul>
    </div>
  );
};

const RouteMarkerRow = ({
  store,
  index,
  marker,
}: {
  store: Livewire<StoreProps, ComputedProps>;
  index: number;
  marker: RouteMarker;
}) => {
  const hasSecondaryInfo =
    marker.routeDistance !== undefined ||
    marker.cutoffTime ||
    marker.goalTime ||
    marker.note;

  return (
    <SimpleRow>
      <div class="block">
        {/* Title row with name and dropdown */}
        <div class="flex w-full justify-between">
          <div>
            <EditableText
              value={marker.name || ""}
              placeholder={`CP ${index + 1}`}
              onChange={(s) => {
                store.$.markers[index].name = s;
                store.$.markers = [...store.$.markers];
              }}
            />
          </div>
          <div>
            <MarkerDropdown store={store} index={index} marker={marker} />
          </div>
        </div>

        {/* Secondary info on separate lines */}
        {hasSecondaryInfo && (
          <div class="space-y-1 text-xs text-gray-600">
            {marker.note && <div class="italic">{marker.note}</div>}
            <div class="flex flex-wrap gap-2">
              {marker.routeDistance !== undefined && (
                <span class="badge badge-xs badge-soft">
                  {marker.routeDistance.toFixed(1)} km
                </span>
              )}
              {marker.goalTime && (
                <span class="badge badge-xs badge-soft">
                  Goal: {formatDateTimeCompact(marker.goalTime)}
                </span>
              )}
              {marker.cutoffTime && (
                <span class="badge badge-xs badge-soft">
                  Cutoff: {formatDateTimeCompact(marker.cutoffTime)}
                </span>
              )}
            </div>
          </div>
        )}
      </div>
    </SimpleRow>
  );
};

const RouteMarkerTable = ({
  store,
}: {
  store: Livewire<StoreProps, ComputedProps>;
}) => {
  return (
    <ul class="list">
      <store.reactive keys="markers">
        {({ markers }: StoreProps) =>
          markers.length === 0 ? (
            <p class="label">No route markers added yet</p>
          ) : (
            <></>
          )
        }
      </store.reactive>

      <store.reactiveEach key="markers">
        {(marker: RouteMarker, idx: number) => (
          <RouteMarkerRow store={store} index={idx} marker={marker} />
        )}
      </store.reactiveEach>
    </ul>
  );
};

const DateTimePicker = ({
  title,
  onChange,
}: {
  title: string;
  onChange: (d: Temporal.Instant) => void;
}) => {
  return (
    <label className="input validator">
      <span className="label">{title} </span>
      <input
        type="datetime-local"
        onBlur={(e: Event) => {
          const target = e.target as HTMLInputElement;
          const val = target.value;
          if (val === "") {
            target.setAttribute("aria-invalid", "true");
            return;
          }
          target.removeAttribute("aria-invalid");

          const dateTime = Temporal.PlainDateTime.from(val).toZonedDateTime(
            Temporal.Now.timeZoneId(),
          );
          onChange(dateTime.toInstant());
        }}
      />
    </label>
  );
};

export function createApp(globalStore: Livewire<GlobalStoreProps>) {
  const store = createStore(globalStore);

  return (
    <main class="mx-auto max-w-5xl bg-base-100 p-4 shadow">
      <div class="md:grid md:grid-cols-2 gap-2">
        <div class="w-full space-y-2">
          <Fieldset title="Timing">
            <p class="label whitespace-normal!">
              Set start and end time to calculate required pacing.
            </p>
            <DateTimePicker
              title={"Start"}
              onChange={(date: Temporal.Instant) => (store.$.startTime = date)}
            />

            <DateTimePicker
              title={"End"}
              onChange={(date: Temporal.Instant) => (store.$.endTime = date)}
            />
          </Fieldset>

          <Fieldset title="Route">
            <input
              id="gpx-files"
              type="file"
              class="hidden"
              accept=".gpx"
              multiple={true}
              onChange={(e: Event) => handleGPXFile(e, store)}
            />

            <p class="label whitespace-normal!">
              Add GPX files for race route. Can be a single file for the entire
              course or split into individual files.
            </p>

            <store.reactive keys="segments">
              {({ segments }: { segments: Segment[] }) =>
                segments.length === 0 ? (
                  ""
                ) : (
                  <div class="inline-flex space-x-1 justify-end">
                    <span class="badge badge-soft badge-xs">
                      {`${segments.length} segments`}
                    </span>
                    <span class="badge badge-soft badge-xs">
                      {`${segments.reduce((xs, x) => x.segmentLength + xs, 0).toFixed()}km`}
                    </span>
                  </div>
                )
              }
            </store.reactive>

            <ul class="list max-h-72 overflow-y-auto space-y-1">
              <store.reactiveEach key="segments">
                {(seg: Segment, idx: number) => (
                  <SimpleRow>
                    <div class="flex items-center gap-2">
                      <span class="flex-1">{seg.title ?? seg.fileName}</span>
                      <span class="badge badge-soft badge-xs tabular-nums">
                        {(seg.segmentLength / 1000).toFixed(0)} km
                      </span>
                      <button
                        onClick={() => {
                          store.$.segments.splice(idx, 1);
                          store.$.segments = [...store.$.segments];
                        }}
                        class="btn btn-soft btn-sm hover:btn-error"
                      >
                        ×
                      </button>
                    </div>
                  </SimpleRow>
                )}
              </store.reactiveEach>
            </ul>

            <div class="flex">
              <button
                // @ts-ignore
                onClick={() => document.querySelector("#gpx-files").click()}
                class="btn btn-neutral"
              >
                Add Route Files
              </button>
            </div>
          </Fieldset>

          <Fieldset title="Markers">
            <RouteMarkerTable store={store} />
          </Fieldset>

          <Fieldset title="Map">
            <div class="h-100">
              <div
                $mount={(el: HTMLElement) => initMap(el, store)}
                class="h-full rounded-box shadow-md"
              />
            </div>
          </Fieldset>

          <store.reactive keys="$valid">
            {({ $valid }: ComputedProps) => (
              <button class="btn" onClick="" disabled={!$valid}>
                Done
              </button>
            )}
          </store.reactive>
        </div>
      </div>

      <button class="btn" onClick={() => (globalStore.$.mode = "PACE_TRACKER")}>
        switch
      </button>

      <details>
        <pre>
          <store.reactive keys={Object.keys(store.$)}>
            {(state: StoreProps & ComputedProps) =>
              JSON.stringify(state, null, 4)
            }
          </store.reactive>
        </pre>
      </details>
    </main>
  );
}

const generateId = () => (1e16 * Math.random()).toString(36);

async function handleGPXFile(
  event: Event,
  store: Livewire<StoreProps, ComputedProps>,
) {
  const files: Array<File> = Array.from(
    (event.target as HTMLInputElement).files,
  );

  const segments: Segment[] = [];
  const routeMarkers: RouteMarker[] = [];

  for (const file of files) {
    const text = await file.text();
    const { tracks, markers } = parseGPX(text);

    for (const t of tracks) {
      segments.push({
        id: (t.id = generateId()),
        fileName: file.name,
        segmentLength: length(t, { units: "meters" }),
        // TODO: simplify geometry
        geometry: t.geometry,
      });
    }

    for (const m of markers) {
      let minDist = Infinity;
      let nearestTrk = null;

      // Find the closest point on any track
      for (const t of tracks) {
        const snapped = nearestPointOnLine(t, m);

        if (snapped.properties.dist < minDist) {
          minDist = snapped.properties.dist;
          nearestTrk = t;
        }
      }

      let routeDistance = null;

      if (nearestTrk) {
        routeDistance = length(
          lineSliceAlong(nearestTrk.geometry, 0, minDist),
          { units: "meters" },
        );
      }

      routeMarkers.push({
        ...m.properties,
        id: generateId(),
        kind: "marker",
        segmentId: nearestTrk?.id?.toString(),
        routeDistance: routeDistance,
        coordinate: m.geometry.coordinates,
        // TODO: snappedCoordinate: nearestPoint,
      });
    }
  }

  store.$.segments = [...store.$.segments, ...segments];
  store.$.markers = [...store.$.markers, ...routeMarkers];

  (event.target as HTMLInputElement).value = "";
}

function initMap(
  node: HTMLElement,
  store: Livewire<StoreProps, ComputedProps>,
) {
  const map = createMap(node);
  let prevSegmentLength = 0;
  console.log("init map", node);

  const unwatch = store.watch(
    ["segments", "markers"],
    ({
      segments,
      markers,
    }: {
      segments: Segment[];
      markers: RouteMarker[];
    }) => {
      if (!map.getMap().getContainer()?.parentNode) {
        return unwatch();
      }

      const trackLines = segments.map((seg) =>
        seg.geometry.coordinates.map(([lng, lat]) => [lat, lng] as LatLngTuple),
      );

      map.setTrack(trackLines, {
        fitBounds: segments.length !== prevSegmentLength,
      });

      // TODO: sucks
      map.setRouteMarkers(markers, {
        onDrag: (
          index: number,
          coord: { lng: number; lat: number },
          marker: L.Marker,
        ) => {
          const snap = snapToNearestTrackSegment(
            store.$.segments,
            [coord.lng, coord.lat],
            map.getMap(),
            50,
          );

          // Update the control with snapped position and segment info
          store.$.markers[index] = {
            ...store.$.markers[index],
            coordinate: snap.coord,
            segmentId: snap.segmentId,
          };
          store.$.markers = [...store.$.markers];

          // Update the marker position to the snapped location
          marker.setLatLng(snap.coord);
        },
      });

      prevSegmentLength = segments.length;
    },
  );

  map.onMapClick(({ lng, lat }: { lng: number; lat: number }) => {
    const popup = L.popup().setLatLng({ lng, lat });

    function addControlHere() {
      const snapResult = snapToNearestTrackSegment(
        store.$.segments,
        [lng, lat],
        map.getMap(),
        200,
      );

      const marker: RouteMarker = {
        id: generateId(),
        kind: "marker",
        coordinate: snapResult.coord,
      };

      store.$.markers = [...store.$.markers, marker];
      popup.close();
    }

    const content = (
      <div>
        <p class="text-md">Add control here?</p>
        <button onClick={addControlHere} class="btn btn-sm">
          Add
        </button>
      </div>
    );

    popup.setContent(content).openOn(map.getMap());
  });
}
