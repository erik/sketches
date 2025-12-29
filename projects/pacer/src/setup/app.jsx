import L from "leaflet";
import "leaflet/dist/leaflet.css";

import { createMap } from "../shared/map.js";
import { ControlPointKind } from "../shared/index.js";

import {
  parseGPX,
  simplifyTrack,
  calculateTrackLength,
} from "../shared/geo/index.js";
import { Livewire } from "../livewire.js";

const createStore = (global) =>
  new Livewire(
    {
      global,
      trackName: "Untitled",
      startTime: null,
      endTime: null,
      trackSegments: [],
      checkpoints: [],
      files: [],

      $valid: ({ trackName, checkpoints, startTime, endTime }) =>
        trackName?.length &&
        checkpoints.length >= 2 &&
        !!startTime &&
        !!endTime,
    },
    global,
  );

const Fieldset = (props, children) => {
  return (
    <fieldset class="fieldset bg-base-200 border border-base-300 p-4">
      <legend class="fieldset-legend">{props.title}</legend>
      {...children}
    </fieldset>
  );
};

const ControlPointRow = ({ store, kind, name, opensAt, closesAt, coord }) => {
  return (
    <tr>
      <td>{name || kind}</td>
      <td></td>
      <td>
        <button class="btn btn-soft btn-sm hover:btn-error">×</button>
      </td>
    </tr>
  );
};

const ControlPointTable = ({ store }) => {
  return (
    <table class="table table-sm">
      <thead>
        <tr>
          <th>Name</th>
          <th></th>
          <th></th>
        </tr>
      </thead>
      <tbody>
        <store.reactiveEach key="checkpoints">
          {(cp) => <ControlPointRow store={store} {...cp} />}
        </store.reactiveEach>
      </tbody>
    </table>
  );
};

export function createApp(globalStore) {
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
                value={store.trackName}
                placeholder="Route Name"
                onInput={(e) => {
                  store.trackName = e.target.value;
                }}
              />
            </label>

            <label className="input validator">
              <span className="label">Start</span>
              <input
                type="datetime-local"
                onBlur={(e) => {
                  const val = e.target.value;
                  if (val === "") {
                    e.target.setAttribute("aria-invalid", "true");
                    return;
                  }
                  e.target.removeAttribute("aria-invalid");
                  store.startTime = new Date(val);
                }}
              />
            </label>

            <label className="input validator">
              <span className="label">Finish</span>
              <input type="datetime-local" required />
            </label>
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

            <ul class="list max-h-72 overflow-y-scroll space-y-1">
              <store.reactiveEach key="trackSegments">
                {(seg) => (
                  <li
                    class="list-row flex items-baseline cursor-move border border-base-300 bg-base-100 hover:bg-base-200 "
                    draggable={true}
                  >
                    <span>⋮⋮</span>
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

          {store.render(["$valid"], ({ $valid }) => (
            <button class="btn" onClick="" disabled={!$valid}>
              Done
            </button>
          ))}
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
          <store.reactive keys={Object.keys(store)}>
            {(s) => JSON.stringify(s, null, 4)}
          </store.reactive>
        </pre>
      </details>
    </main>
  );
}

async function handleGPXFile(event, store) {
  const files = Array.from(event.target.files);
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

  store.trackSegments = [...store.trackSegments, ...newSegments];
}

function initMap(node, store) {
  const map = createMap(node);

  const unwatch = store.watch(
    ["trackSegments", "checkpoints"],
    ({ trackSegments, checkpoints }) => {
      if (!map.getMap()._container?.parentNode) {
        return unwatch();
      }

      console.log(checkpoints);

      const line = trackSegments.map((seg) => seg.coords);
      map.showTrack(line);
      map.showCheckpoints(checkpoints);
    },
  );

  map.onMapClick((coord) => {
    const popup = L.popup().setLatLng([coord[1], coord[0]]);

    function addCheckpointAt(coord) {
      const control = {
        kind: ControlPointKind.Control,
        name: null,
        km: 0,
        opensAt: null,
        closesAt: null,
        coord: { lng: coord[0], lat: coord[1] },
      };

      store.checkpoints = [...store.checkpoints, control];
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
