import L, { LatLngTuple } from "leaflet";
import "leaflet/dist/leaflet.css";

import { createMap } from "../shared/map.js";
import { RouteMarker, Segment } from "../shared/index.js";

import { snapToNearestTrackSegment } from "../shared/geo.js";
import { parseGPX } from "../shared/gpx.js";
import { Livewire, type Children } from "../livewire.js";
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

const createStore = (
  global: Livewire<GlobalStoreProps>,
): Livewire<StoreProps, ComputedProps> => {
  const store = new Livewire<StoreProps, ComputedProps>({
    trackName: "Untitled",
    startTime: null,
    endTime: null,
    segments: [],
    markers: [],
  }).compute(
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
        class="input input-sm"
      />
    ) : (
      <span
        onClick={() => (store.$.editing = true)}
        class="cursor-pointer text-sm hover:bg-base-100 p-2"
      >
        {textValue || placeholder}
      </span>
    ),
  );
};

const SortableRow = (
  { store, index, watchKey }: { store: any; index: number; watchKey: string },
  ...children: Children
) => {
  const reorderItems = (i: number, j: number) => {
    if (i !== j && i >= 0 && j >= 0) {
      const xs = [...store.$[watchKey]];
      const [x] = xs.splice(i, 1);
      xs.splice(j, 0, x);
      store.$[watchKey] = xs;
    }
  };

  const dropStyle = ["ring-2", "ring-inset", "ring-primary", "bg-primary/20"];
  const cleanupDrag = () => {
    document.querySelectorAll(".sortable-row").forEach((row) => {
      dropStyle.forEach((s) => row.classList.remove(s));
    });
  };

  // Mouse/drag event handlers
  const handleDragStart = (e: DragEvent) => {
    e.dataTransfer.setData("text/plain", index.toString());
    e.dataTransfer.effectAllowed = "move";
  };

  const onDragEnter = (e: DragEvent) => {
    const target = e.target as HTMLElement;

    const node = target.classList.contains("sortable-row")
      ? target
      : target.closest(".sortable-row");

    dropStyle.forEach((s) => node?.classList.add(s));
  };

  const onDragLeave = (e: DragEvent) => {
    const target = e.target as HTMLElement;
    if (target.classList.contains("sortable-row")) {
      dropStyle.forEach((s) => target.classList.remove(s));
    }
  };

  const handleDragOver = (e: DragEvent) => {
    e.preventDefault();
    e.dataTransfer.dropEffect = "move";
  };

  const handleDrop = (e: DragEvent) => {
    e.preventDefault();
    const fromIndex = parseInt(e.dataTransfer.getData("text/plain"));
    const toIndex = index;

    reorderItems(fromIndex, toIndex);
    cleanupDrag();
  };

  const handleDragEnd = (e: DragEvent) => cleanupDrag();
  return (
    <li
      class="sortable-row list-row flex items-baseline border border-base-300 bg-base-100 hover:bg-base-200 active:opacity-50"
      draggable={true}
      onDragStart={handleDragStart}
      onDragOver={handleDragOver}
      onDrop={handleDrop}
      onDragEnd={handleDragEnd}
      onDragEnter={onDragEnter}
      onDragLeave={onDragLeave}
    >
      <div class="cursor-grab active:cursor-grabbing">⠿</div>
      {...children}
    </li>
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
  return (
    <SortableRow store={store} index={index} watchKey="markers">
      <EditableText
        value={marker.name || ""}
        placeholder={`CP ${index + 1}`}
        onChange={(s) => {
          store.$.markers[index].name = s;
          store.$.markers = [...store.$.markers];
        }}
      />
      <span class="flex-1" />
      <button
        onClick={() => {
          store.$.markers.splice(index, 1);
          store.$.markers = [...store.$.markers];
        }}
        class="btn btn-soft btn-sm hover:btn-error"
      >
        ...
      </button>
    </SortableRow>
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
                  <SortableRow store={store} index={idx} watchKey="segments">
                    {seg.title ?? seg.fileName}
                    <span class="flex-1" />

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
                  </SortableRow>
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
