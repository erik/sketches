import L from "leaflet";
import "leaflet/dist/leaflet.css";

import { createMap } from "../shared/map.js";
import { Segment, type ControlPoint } from "../shared/index.js";

import {
  parseGPX,
  simplifyTrack,
  calculateTrackLength,
  snapToNearestTrackSegment,
} from "../shared/geo.js";
import { Livewire } from "../livewire.js";

type StoreProps = {
  trackName: string;
  startTime?: Date;
  endTime?: Date;
  segments: Segment[];
  controls: ControlPoint[];
};

const createStore = (global: Livewire<any>): Livewire<StoreProps> => {
  const store = new Livewire<StoreProps>(
    {
      trackName: "Untitled",
      startTime: null,
      endTime: null,
      segments: [],
      controls: [],
    },
    global,
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
    <fieldset class="fieldset bg-base-200 border border-base-300 p-4">
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

const ControlPointRow = ({ store, index, cp }) => {
  return (
    <li
      class="list-row flex items-baseline border border-base-300 bg-base-100 hover:bg-base-200 "
      draggable={true}
    >
      <span class="cursor-move">⠿</span>
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
    </li>
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

export function createApp(globalStore: Livewire<any>) {
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

            <DateTimePicker
              title={"Start"}
              onChange={(date) => (store.$.startTime = date)}
            />

            <DateTimePicker
              title={"End"}
              onChange={(date) => (store.$.endTime = date)}
            />
          </Fieldset>
          <Fieldset title="GPX Files">
            <input
              type="file"
              id="gpxFiles"
              class="file-input"
              accept=".gpx"
              multiple={true}
              onChange={(e) => handleGPXFile(e, store)}
            />

            <p class="label">
              Add and arrange any GPX files related to this route.
            </p>

            <ul class="list max-h-72 overflow-y-auto space-y-1">
              <store.reactiveEach key="segments">
                {(seg) => (
                  <li
                    class="list-row flex items-baseline cursor-move border border-base-300 bg-base-100 hover:bg-base-200 "
                    draggable={true}
                  >
                    <span class="text-xs">⋮⋮</span>
                    {seg.name}
                    <span class="flex-1" />
                    <span class="tabular-nums">{seg.length.toFixed(0)} km</span>
                    <button class="btn btn-soft btn-sm hover:btn-error">
                      ×
                    </button>
                  </li>
                )}
              </store.reactiveEach>
            </ul>
          </Fieldset>
          <Fieldset title="Checkpoints">
            <ControlPointTable store={store} />
          </Fieldset>

          <store.reactive keys="$valid">
            {({ $valid }) => (
              <button class="btn" onClick="" disabled={!$valid}>
                Done
              </button>
            )}
          </store.reactive>
        </div>

        <div class="w-full h-full p-4">
          <div
            $mount={(el) => initMap(el, store)}
            class="h-full rounded-box shadow-md"
          />
        </div>
      </div>

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

async function handleGPXFile(event, store: Livewire<StoreProps>) {
  const files: Array<File> = Array.from(event.target.files);
  const newSegments = [];

  for (const file of files) {
    const text = await file.text();
    const coords = parseGPX(text);

    if (coords.length === 0) {
      console.log(`No valid track data found in ${file.name}`, "error");
      continue;
    }

    newSegments.push({
      id: Math.random().toString(36).substring(2, 10),
      name: file.name,
      coords: simplifyTrack(coords),
      length: calculateTrackLength(coords),
    });
  }

  // Reset form input
  event.target.value = "";
  store.$.segments = [...store.$.segments, ...newSegments];
}

function initMap(node, store: Livewire<StoreProps>) {
  const map = createMap(node);

  const unwatch = store.watch(
    ["segments", "controls"],
    ({ segments, controls }) => {
      if (!map.getMap()._container?.parentNode) {
        return unwatch();
      }

      const line = segments.map((seg) => seg.coords);
      map.showTrack(line);
      map.showCheckpoints(controls, {
        onDragEnd: (index, coord) => {
          store.$.controls[index].coord = coord;
          store.$.controls = [...store.$.controls];
        },
      });
    },
  );

  map.onMapClick((coord) => {
    const popup = L.popup().setLatLng([coord[1], coord[0]]);

    function addCheckpointAt(coord) {
      // Get all track segments for snapping
      const trackSegments = store.$.segments.map((seg) => seg.coords);

      // Snap to nearest track segment if within reasonable distance
      const snapResult = snapToNearestTrackSegment(
        trackSegments,
        coord,
        map.getMap(),
        200, // 20 pixel threshold
      );

      const control: ControlPoint = {
        kind: "cp",
        name: null,
        closesAt: null,
        coord: { lng: snapResult.coord[0], lat: snapResult.coord[1] },
        anchorSegmentId: snapResult.segmentId, // null if not snapped to any segment
      };

      store.$.controls = [...store.$.controls, control];
      popup.close();
    }

    const content = (
      <div>
        <p class="text-md">Add control here?</p>
        <button onClick={() => addCheckpointAt(coord)} class="btn btn-sm">
          Add
        </button>
      </div>
    );

    popup.setContent(content).openOn(map.getMap());
  });
}
