import L from "leaflet";
import "leaflet/dist/leaflet.css";

import { createMap } from "../shared/map.js";
import { Segment, type OldControlPoint } from "../shared/index.js";

import {
  parseGPX,
  simplifyTrack,
  calculateTrackLength,
  snapToNearestTrackSegment,
} from "../shared/geo.js";
import { Livewire } from "../livewire.js";
import { GlobalStoreProps } from "../main.jsx";

type StoreProps = {
  trackName: string;
  startTime?: Date;
  endTime?: Date;
  segments: Segment[];
  controls: OldControlPoint[];
};

type ComputedProps = {
  $valid: boolean;
};

const createStore = (
  global: Livewire<GlobalStoreProps>,
): Livewire<StoreProps, ComputedProps> => {
  const store = new Livewire<StoreProps, ComputedProps>(
    {
      trackName: "Untitled",
      startTime: null,
      endTime: null,
      segments: [],
      controls: [],
    },
    { parent: global },
  );

  store.compute(
    "$valid",
    ({ trackName, controls, startTime, endTime }) =>
      trackName?.length && controls.length >= 2 && !!startTime && !!endTime,
  );

  return store;
};

const Fieldset = (props, children) => {
  return (
    <fieldset class="fieldset bg-base-200 border border-base-300 p-2">
      <legend class="fieldset-legend">{props.title}</legend>
      {...children}
    </fieldset>
  );
};

const EditableText = ({ onChange, value, placeholder }) => {
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
        onBlur={(e) => {
          store.$.editing = false;
          store.$.textValue = e.target.value;
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

const SortableRow = ({ store, index, onUpdate }, children) => {
  const reorderItems = (fromIndex, toIndex) => {
    if (fromIndex !== toIndex && fromIndex >= 0 && toIndex >= 0) {
      const newArray = [...store.$[onUpdate]];
      const [movedItem] = newArray.splice(fromIndex, 1);
      newArray.splice(toIndex, 0, movedItem);
      store.$[onUpdate] = newArray;
    }
  };

  const dropStyle = ["ring-2", "ring-inset", "ring-primary", "bg-primary/20"];

  const cleanupDrag = () => {
    document.querySelectorAll(".sortable-row").forEach((row) => {
      dropStyle.forEach((s) => row.classList.remove(s));
    });
  };

  // Mouse/drag event handlers
  const handleDragStart = (e) => {
    e.dataTransfer.setData("text/plain", index);
    e.dataTransfer.effectAllowed = "move";
  };

  const onDragEnter = (e) => {
    const node = e.target.classList.contains("sortable-row")
      ? e.target
      : e.target.closest(".sortable-row");

    dropStyle.forEach((s) => node.classList.add(s));
  };

  const onDragLeave = (e) => {
    if (e.target.classList.contains("sortable-row")) {
      dropStyle.forEach((s) => e.target.classList.remove(s));
    }
  };

  const handleDragOver = (e) => {
    e.preventDefault();
    e.dataTransfer.dropEffect = "drop";
  };

  const handleDrop = (e) => {
    e.preventDefault();
    const fromIndex = parseInt(e.dataTransfer.getData("text/plain"));
    const toIndex = index;

    reorderItems(fromIndex, toIndex);
    cleanupDrag();
  };

  const handleDragEnd = (e) => {
    cleanupDrag();
  };

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

const ControlPointRow = ({ store, index, cp }) => {
  return (
    <SortableRow store={store} index={index} onUpdate="controls">
      <EditableText
        value={cp.name}
        placeholder={`CP ${index + 1}`}
        onChange={(s) => {
          store.$.controls[index].name = s;
          store.$.controls = [...store.$.controls];
        }}
      />
      <span class="flex-1" />
      <button
        onClick={() => {
          store.$.controls.splice(index, 1);
          store.$.controls = [...store.$.controls];
        }}
        class="btn btn-soft btn-sm hover:btn-error"
      >
        ...
      </button>
    </SortableRow>
  );
};

const ControlPointTable = ({ store }) => {
  return (
    <ul class="list">
      <store.reactive keys="controls">
        {({ controls }) =>
          controls.length === 0 ? (
            <p class="label">No controls added yet</p>
          ) : (
            <></>
          )
        }
      </store.reactive>

      <store.reactiveEach key="controls">
        {(cp, idx) => <ControlPointRow store={store} index={idx} cp={cp} />}
      </store.reactiveEach>
    </ul>
  );
};

const DateTimePicker = ({ title, onChange }) => {
  return (
    <label className="input validator">
      <span className="label">{title} </span>
      <input
        type="datetime-local"
        onBlur={(e) => {
          const val = e.target.value;
          if (val === "") {
            e.target.setAttribute("aria-invalid", "true");
            return;
          }

          e.target.removeAttribute("aria-invalid");
          onChange(new Date(val));
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
          <Fieldset title="Details">
            <label className="input validator">
              <span className="label">Name</span>
              <input
                required
                minLength={1}
                type="text"
                id="routeName"
                value={store.$.trackName}
                placeholder="Transcontinental no11"
                onInput={(e) => (store.$.trackName = e.target.value)}
              />
            </label>
          </Fieldset>

          <Fieldset title="Timing">
            <p class="label whitespace-normal!">
              Set start and end time to calculate required pacing.
            </p>
            <DateTimePicker
              title={"Start"}
              onChange={(date) => (store.$.startTime = date)}
            />

            <DateTimePicker
              title={"End"}
              onChange={(date) => (store.$.endTime = date)}
            />
          </Fieldset>

          <Fieldset title="Route">
            <input
              id="gpx-files"
              type="file"
              class="hidden"
              accept=".gpx"
              multiple={true}
              onChange={(e) => handleGPXFile(e, store)}
            />

            <p class="label whitespace-normal!">
              Add GPX files for race route. Can be a single file for the entire
              course or split into individual files.
            </p>

            <store.reactive keys="segments">
              {({ segments }) =>
                segments.length === 0 ? (
                  ""
                ) : (
                  <div class="inline-flex space-x-1 justify-end">
                    <span class="badge badge-soft badge-xs">
                      {`${segments.length} segments`}
                    </span>
                    <span class="badge badge-soft badge-xs">
                      {`${segments.reduce((xs, x) => x.length + xs, 0).toFixed()}km`}
                    </span>
                  </div>
                )
              }
            </store.reactive>

            <ul class="list max-h-72 overflow-y-auto space-y-1">
              <store.reactiveEach key="segments">
                {(seg: Segment, idx) => (
                  <SortableRow store={store} index={idx} onUpdate="segments">
                    {seg.title ?? seg.fileName}
                    <span class="flex-1" />

                    <span class="badge badge-soft badge-xs tabular-nums">
                      {seg.segmentLength.toFixed(0)} km
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

          <div class="">
            <Fieldset title="Controls">
              <div
                $mount={(el) => initMap(el, store)}
                class="h-100 rounded-box shadow-md"
              />
              <ControlPointTable store={store} />
            </Fieldset>
          </div>

          <store.reactive keys="$valid">
            {({ $valid }) => (
              <button class="btn" onClick="" disabled={!$valid}>
                Done
              </button>
            )}
          </store.reactive>
        </div>
      </div>

      <button
        class="btn"
        onClick={() => globalStore.dispatch("setMode", "PACE_TRACKER")}
      >
        switch
      </button>

      <details>
        <pre>
          <store.reactive keys={Object.keys(store.$)}>
            {(state) => JSON.stringify(state, null, 4)}
          </store.reactive>
        </pre>
      </details>
    </main>
  );
}

async function handleGPXFile(
  event,
  store: Livewire<StoreProps, ComputedProps>,
) {
  const files: Array<File> = Array.from(event.target.files);
  const newSegments: Segment[] = [];

  for (const file of files) {
    const text = await file.text();
    const coords = parseGPX(text);

    if (coords.length === 0) {
      console.log(`No valid track data found in ${file.name}`, "error");
      continue;
    }

    newSegments.push({
      id: Math.random().toString(36).substring(2, 10),
      fileName: file.name,
      geometry: simplifyTrack(coords),
      segmentLength: calculateTrackLength(coords),
    });
  }

  // Reset form input
  event.target.value = "";
  store.$.segments = [...store.$.segments, ...newSegments];
}

function initMap(
  node: HTMLElement,
  store: Livewire<StoreProps, ComputedProps>,
) {
  const map = createMap(node);
  let prevSegmentLength = 0;

  const unwatch = store.watch(
    ["segments", "controls"],
    ({ segments, controls }) => {
      if (!map.getMap().getContainer()?.parentNode) {
        return unwatch();
      }

      const line = segments.map((seg) => seg.geometry);

      // @ts-ignore TODO fixme
      map.setTrack(line, {
        fitBounds: segments.length !== prevSegmentLength,
      });

      map.setControlPoints(controls, {
        onDragEnd: (index, coord, marker) => {
          const snap = snapToNearestTrackSegment(
            store.$.segments,
            [coord.lng, coord.lat],
            map.getMap(),
            50,
          );

          // Update the control with snapped position and segment info
          store.$.controls[index] = {
            ...store.$.controls[index],
            coord: {
              lng: snap.coord[0],
              lat: snap.coord[1],
            },
            anchorSegmentId: snap.segmentId,
          };
          store.$.controls = [...store.$.controls];

          // Update the marker position to the snapped location
          marker.setLatLng(snap.coord);
        },
      });

      prevSegmentLength = segments.length;
    },
  );

  map.onMapClick(({ lng, lat }) => {
    const popup = L.popup().setLatLng({ lng, lat });

    function addControlHere() {
      const snapResult = snapToNearestTrackSegment(
        store.$.segments,
        { lng, lat },
        map.getMap(),
        200,
      );

      const control: OldControlPoint = {
        kind: "cp",
        name: null,
        closesAt: null,
        coord: { lng: snapResult.coord[0], lat: snapResult.coord[1] },
        anchorSegmentId: snapResult.segmentId,
      };

      store.$.controls = [...store.$.controls, control];
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
