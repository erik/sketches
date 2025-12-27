import "./style.css";
import { createStore, createInitialState, generateId } from "./state.js";
import {
  parseGPX,
  simplifyTrack,
  trackLength,
  getCoordAtKm,
  sortCheckpointsByDistance,
  snapToTrack,
  findCheckpointOnTrack,
  calculateDistance,
} from "./geo.js";
import {
  saveRouteToURL,
  loadRouteFromURL,
  saveTracking,
  loadTracking,
} from "./storage.js";
import { createMap } from "./map.js";
import L from "leaflet";
import {
  calculateCurrentPace,
  calculateRequiredPace,
  calculateEstimatedArrival,
  calculateTimeAheadBehind,
  calculateSummaryStats,
  getCurrentCheckpointIndex,
  formatTimeDifference,
  getTimeDifferenceHours,
} from "./pace.js";
import {
  showInlineMessage,
  formatDateTime,
  formatCutoffTime,
  getStartInfo,
  formatDateTimeLocal,
  scrollToElement,
  canSaveRoute,
  findMissingCutoffs,
  createModal,
  calculateTimeRemaining,
  formatTimeRemaining,
} from "./helpers.js";
import {
  calculateCheckpointMetrics,
  canCheckIn,
  findNextCheckpoint,
  getCutoffTimeForCheckpoint,
  validateClearCheckpoint,
  validateSequentialCheckIn,
} from "./checkpoint-ops.js";

// Constants for checkpoint IDs to avoid stringly-typed issues
const CHECKPOINT_IDS = {
  START: "start",
  FINISH: "finish",
};

// Create the app store
const store = createStore(createInitialState());

// Map instance (created on demand)
let mapInstance = null;

// Helper function to ensure start/finish checkpoints exist when track is present
function updateStartFinishCheckpoints(existingCheckpoints, track) {
  const checkpoints = [...existingCheckpoints];

  // Only create/update start/finish if we have a track
  if (track.length === 0) {
    return checkpoints;
  }

  // Find existing start/finish
  let startCp = checkpoints.find((cp) => cp.id === CHECKPOINT_IDS.START);
  let finishCp = checkpoints.find((cp) => cp.id === CHECKPOINT_IDS.FINISH);

  const totalLength = trackLength(track);

  // Update or create start checkpoint
  if (startCp) {
    startCp.km = 0;
    startCp.coord = track[0];
  } else {
    startCp = {
      id: CHECKPOINT_IDS.START,
      name: "Start",
      km: 0,
      coord: track[0],
      // Default to current time as placeholder for event start
      cutoff: new Date().toISOString().slice(0, 16) + ":00",
    };
    checkpoints.unshift(startCp);
  }

  // Update or create finish checkpoint
  if (finishCp) {
    finishCp.km = totalLength;
    finishCp.coord = track[track.length - 1];
  } else {
    // Calculate default finish cutoff: start time + 7 days
    const startCutoff = checkpoints.find((cp) => cp.id === "start")?.cutoff;
    const defaultFinishTime = startCutoff
      ? new Date(new Date(startCutoff).getTime() + 7 * 24 * 60 * 60 * 1000)
          .toISOString()
          .slice(0, 16) + ":00"
      : new Date(Date.now() + 7 * 24 * 60 * 60 * 1000)
          .toISOString()
          .slice(0, 16) + ":00";

    finishCp = {
      id: CHECKPOINT_IDS.FINISH,
      name: "Finish",
      km: totalLength,
      coord: track[track.length - 1],
      cutoff: defaultFinishTime,
    };
    checkpoints.push(finishCp);
  }

  return checkpoints;
}

// Initialize the app
async function init() {
  // Try to load route from URL
  const route = await loadRouteFromURL();

  if (route) {
    // Tracking mode - route found in URL
    // Ensure start/finish checkpoints exist
    const checkpoints = updateStartFinishCheckpoints(
      route.checkpoints || [],
      route.track || [],
    );

    store.update({
      mode: "tracking",
      route: {
        ...route,
        checkpoints,
      },
      tracking: loadTracking(route.created) || {
        routeId: route.created,
        arrivals: {},
      },
    });
    renderTrackingMode();
  } else {
    // Setup mode - no route in URL
    store.update({ mode: "setup" });
    renderSetupMode();
  }

  // Subscribe to state changes with selective updates
  let lastState = store.get();
  store.subscribe((state) => {
    // Mode change - full re-render
    if (state.mode !== lastState.mode) {
      if (state.mode === "setup") {
        renderSetupMode();
      } else {
        renderTrackingMode();
      }
      lastState = state;
      return;
    }

    // In setup mode, update only what changed
    if (state.mode === "setup") {
      // Segments or track changed - need full re-render to show/hide checkpoint list
      // fixme: this is stupid and bad. need real rendering solution
      if (
        state.route.segments !== lastState.route.segments ||
        state.route.track !== lastState.route.track
      ) {
        console.log("Re-rendering setup mode due to segments/track change");
        renderSetupMode();
        lastState = state;
        return;
      }

      // Checkpoints changed - update checkpoints list
      if (state.route.checkpoints !== lastState.route.checkpoints) {
        const checkpointsList = document.getElementById("checkpointsList");
        if (checkpointsList) {
          checkpointsList.innerHTML = renderCheckpointsList(state);
          setupCheckpointListeners();
        }

        // Update map markers
        if (mapInstance) {
          mapInstance.showCheckpoints(state.route.checkpoints, {
            draggable: true,
            draggableStartFinish: state.route.track.length === 0,
            onDragEnd: (checkpoint, newCoord) => {
              if (state.route.track.length > 0) {
                const snapped = snapToTrack(state.route.track, newCoord);
                if (snapped) {
                  updateCheckpoint(checkpoint.id, {
                    coord: snapped.coord,
                    km: snapped.km,
                  });
                }
              } else {
                updateCheckpoint(checkpoint.id, { coord: newCoord });
              }
            },
          });
        }
      }

      // Route name or checkpoints changed - update save button
      if (
        state.route.name !== lastState.route.name ||
        state.route.checkpoints !== lastState.route.checkpoints
      ) {
        const saveBtn = document.getElementById("saveRoute");
        if (saveBtn) {
          saveBtn.disabled = !canSaveRoute(state);
        }

        // Update validation message
        const saveSection = saveBtn?.parentElement;
        if (saveSection) {
          // fixme: what the fuck is this
          const existingHint = saveSection.querySelector(
            '.hint[style*="dc2626"]',
          );
          if (!canSaveRoute(state)) {
            if (!existingHint) {
              // fixme: we have a helper for this
              const hint = document.createElement("p");
              hint.className = "hint";
              hint.style.color = "#dc2626";
              hint.textContent =
                "Need: route name, start/finish checkpoints, and cutoff times";
              saveSection.appendChild(hint);
            }
          } else if (existingHint) {
            existingHint.remove();
          }
        }
      }
    }

    // In tracking mode, update tracking display
    if (state.mode === "tracking") {
      if (state.tracking !== lastState.tracking) {
        renderTrackingMode();
      }
    }

    lastState = state;
  });
}

// ============================================================================
// SETUP MODE
// ============================================================================

function renderSetupMode() {
  const state = store.get();
  const app = document.querySelector("#app");

  // FIXME: insane to hard code all of this. we want to support a metric/imperial setting
  // FIXME: inline style or separate CSS. pick one and stick to it!
  app.innerHTML = `
    <div class="setup-container">
      <h1>Pacer - Setup Route</h1>

      <div class="setup-layout">
        <div class="setup-form">
          <div class="setup-section">
            <h3>Track & Checkpoints</h3>

            <input type="file" id="gpxFiles" accept=".gpx" multiple />
            ${
              state.route.track.length > 0
                ? `<p class="success">✓ ${trackLength(state.route.track).toFixed(0)} km track (${state.route.segments.length} segment${state.route.segments.length !== 1 ? "s" : ""})</p>`
                : '<p class="hint">Upload GPX file(s) or click map to place start/finish</p>'
            }

            <div id="segmentsList">
              ${renderSegmentsList(state)}
            </div>

            ${
              state.route.checkpoints.length > 0
                ? `<div id="checkpointsList">${renderCheckpointsList(state)}</div>`
                : '<p class="hint">No checkpoints yet. Upload GPX or click map to place start.</p>'
            }

            <input type="text" id="routeName" value="${state.route.name}" placeholder="Route name">

            <button id="saveRoute" class="primary" ${canSaveRoute(state) ? "" : "disabled"} style="width: 100%; margin-top: 1rem;">
              Save & Generate URL
            </button>

            ${!canSaveRoute(state) ? '<p class="hint" style="color: #dc2626;">Need: route name, start/finish checkpoints, and cutoff times</p>' : ""}
          </div>
        </div>

        <div class="setup-map">
          <div id="map" class="map-container-setup"></div>
        </div>
      </div>
    </div>
  `;

  // Event listeners
  document.getElementById("routeName").addEventListener("input", (e) => {
    store.updateNestedSilent("route.name", e.target.value);
  });

  document
    .getElementById("gpxFiles")
    .addEventListener("change", handleGPXUpload);

  // fixme: why would this be null???
  document.getElementById("saveRoute")?.addEventListener("click", saveRoute);

  // Setup drag and drop for segments
  setupSegmentDragAndDrop();

  // Initialize map
  initSetupMap();
}

function initSetupMap() {
  const state = store.get();

  // Clean up existing map
  if (mapInstance) {
    mapInstance.destroy();
  }

  // Create new map
  mapInstance = createMap("map");

  // Show track if available
  if (state.route.track.length > 0) {
    mapInstance.showTrack(state.route.track);
  }

  // Show checkpoints
  mapInstance.showCheckpoints(state.route.checkpoints, {
    draggable: true,
    draggableStartFinish: state.route.track.length === 0, // Allow dragging start/finish if no track
    // fixme: why is this logic in the init function? it's doing too much
    onDragEnd: (checkpoint, newCoord) => {
      // Snap to track if available
      if (state.route.track.length > 0) {
        const snapped = snapToTrack(state.route.track, newCoord);
        if (snapped) {
          updateCheckpoint(checkpoint.id, {
            coord: snapped.coord,
            km: snapped.km,
          });
        }
      } else {
        // No track - allow manual positioning
        updateCheckpoint(checkpoint.id, { coord: newCoord });
      }
    },
    onClick: (checkpoint) => {
      // Select checkpoint for editing
      store.updateNestedSilent("ui.selectedCheckpointId", checkpoint.id);
    },
  });

  // Add click handler to add checkpoints (with tooltip confirmation)
  // fixme: too much shit in the init function. too much shit in this click handler too!!!
  mapInstance.onMapClick((coord) => {
    const sorted = sortCheckpointsByDistance(state.route.checkpoints);
    const hasStart = sorted.some((cp) => cp.id === CHECKPOINT_IDS.START);
    const hasFinish = sorted.some((cp) => cp.id === CHECKPOINT_IDS.FINISH);

    if (state.route.track.length > 0) {
      const snapped = findCheckpointOnTrack(state.route.track, coord);

      // Show tooltip/popup to confirm
      const popup = L.popup()
        .setLatLng([snapped.coord[1], snapped.coord[0]])
        .setContent(
          `
          <div style="text-align: center;">
            <p style="margin: 0 0 8px 0;"><strong>Add checkpoint here?</strong></p>
            <p style="margin: 0 0 8px 0; font-size: 0.9em;">Distance: ${snapped.km.toFixed(1)} km</p>
            <button id="confirmAddCheckpoint" style="padding: 4px 12px; cursor: pointer;">Add Checkpoint</button>
          </div>
        `,
        )
        .openOn(mapInstance.getMap());

      // Wait for button to be added to DOM
      // fixme: this doesn't work.
      setTimeout(() => {
        document
          .getElementById("confirmAddCheckpoint")
          ?.addEventListener("click", () => {
            addCheckpointAt(snapped.coord, snapped.km);
            mapInstance.getMap().closePopup();
          });
      }, 0);
    } else {
      // No track - add start first, then finish, then intermediate
      let checkpointType = !hasStart
        ? "start"
        : !hasFinish
          ? "finish"
          : "intermediate";

      let label = {
        start: "Start",
        finish: "Finish",
        intermediate: "Checkpoint",
      }[checkpointType];

      const popup = L.popup()
        .setLatLng([coord[1], coord[0]])
        .setContent(
          `
          <div style="text-align: center;">
            <p style="margin: 0 0 8px 0;"><strong>Add ${label} here?</strong></p>
            <button id="confirmAddCheckpoint" style="padding: 4px 12px; cursor: pointer;">Add ${label}</button>
          </div>
        `,
        )
        .openOn(mapInstance.getMap());

      setTimeout(() => {
        document
          .getElementById("confirmAddCheckpoint")
          ?.addEventListener("click", () => {
            addCheckpointAtClick(coord, checkpointType);
            mapInstance.getMap().closePopup();
          });
      }, 0);
    }
  });

  // Fit map to content
  mapInstance.fitToContent();
}

function renderSegmentsList(state) {
  if (state.route.segments.length === 0) {
    return "";
  }

  return `
    <ul class="segments-list">
      ${state.route.segments
        .map(
          (seg, idx) => `
        <li class="segment-item" data-id="${seg.id}" draggable="true">
          <span class="drag-handle">⋮⋮</span>
          <span class="segment-name">${idx + 1}. ${seg.name.replace(".gpx", "")} • ${seg.length.toFixed(0)}km</span>
          <button class="delete-segment" data-id="${seg.id}">×</button>
        </li>
      `,
        )
        .join("")}
    </ul>
  `;
}

function renderCheckpointsList(state) {
  // Ensure start/finish exist
  const checkpoints = updateStartFinishCheckpoints(
    state.route.checkpoints,
    state.route.track,
  );

  const sorted = sortCheckpointsByDistance(checkpoints);
  return `
    <table class="checkpoints-table">
      <thead>
        <tr>
          <th><span class="short-label">≡</span><span class="full-label"></span></th>
          <th>Name</th>
          <th><span class="full-label">km</span><span class="short-label">km</span></th>
          <th>Cutoff</th>
          <th></th>
        </tr>
      </thead>
      <tbody id="checkpointsTableBody">
        ${sorted
          .map((cp) => {
            const isStartOrFinish =
              cp.id === CHECKPOINT_IDS.START || cp.id === CHECKPOINT_IDS.FINISH;
            // Start/finish are readonly when GPX track exists, editable otherwise
            const kmReadonly = state.route.track.length > 0 && isStartOrFinish;
            const nameReadonly = isStartOrFinish; // Name always readonly for start/finish

            const missingCutoff = !cp.cutoff && cp.cutoffHours == null;
            return `
          <tr data-id="${cp.id}" class="${isStartOrFinish ? "fixed-cp" : ""} ${missingCutoff ? "missing-cutoff" : ""}" draggable="${!isStartOrFinish}">
            <td class="drag-cell">${isStartOrFinish ? "" : '<span class="drag-handle">⋮⋮</span>'}</td>
            <td><input type="text" value="${cp.name}" class="cp-name" data-id="${cp.id}" ${nameReadonly ? "readonly" : ""}></td>
            <td><input type="number" value="${cp.km}" step="0.1" class="cp-km" data-id="${cp.id}" ${kmReadonly ? "readonly" : ""}></td>
            <td>
              ${
                cp.cutoff
                  ? `
                <input type="datetime-local" value="${cp.cutoff.slice(0, 16)}" class="cp-cutoff" data-id="${cp.id}" required>
                ${isStartOrFinish && cp.id === "start" ? '<div style="font-size: 0.75em; color: #059669; margin-top: 0.25rem;">Event start time</div>' : ""}
              `
                  : cp.cutoffHours != null
                    ? `
                <input type="number" value="${cp.cutoffHours}" step="0.5" class="cp-hours" data-id="${cp.id}" placeholder="Hours from start" required>
              `
                    : `
                <select class="cp-cutoff-type" data-id="${cp.id}">
                  <option value="">⚠ Required</option>
                  <option value="absolute">Absolute Time</option>
                  <option value="relative">Hours from Start</option>
                </select>
              `
              }
            </td>
            <td>
              ${isStartOrFinish ? '<span class="auto-label">Auto</span>' : `<button class="delete-cp" data-id="${cp.id}">Delete</button>`}
            </td>
          </tr>
        `;
          })
          .join("")}
      </tbody>
    </table>
  `;
}

async function handleGPXUpload(e) {
  const files = Array.from(e.target.files);
  if (files.length === 0) return;

  const state = store.get();
  const newSegments = [];

  for (const file of files) {
    const text = await file.text();
    const coords = parseGPX(text);

    if (coords.length === 0) {
      console.warn(`No track data found in ${file.name}`);
      continue;
    }

    // Simplify the track
    const simplified = simplifyTrack(coords, 0.001);
    console.log(
      `${file.name}: Simplified from ${coords.length} to ${simplified.length} points`,
    );

    newSegments.push({
      id: generateId(),
      name: file.name,
      coords: simplified,
      length: trackLength(simplified),
    });
  }

  if (newSegments.length === 0) {
    alert("No valid track data found in uploaded files");
    return;
  }

  const allSegments = [...state.route.segments, ...newSegments];

  // Preserve individual segments for display and provide combined track for calculations
  const combinedTrack = combineSegments(allSegments);

  // Update or create start/finish checkpoints using the combined track
  const checkpoints = updateStartFinishCheckpoints(
    state.route.checkpoints,
    combinedTrack,
  );

  store.update({
    route: {
      ...state.route,
      segments: allSegments,
      track: combinedTrack,
      checkpoints,
    },
  });

  // Clear the file input
  e.target.value = "";

  // Re-setup listeners after render
  // fixme: this is absurd.
  setTimeout(() => setupSegmentDragAndDrop(), 0);
}

// fixme: don't want this
function combineSegments(segments) {
  if (segments.length === 0) return [];

  // Concatenate all segment coordinates
  return segments.flatMap((seg) => seg.coords);
}

function deleteSegment(segmentId) {
  const state = store.get();
  const segments = state.route.segments.filter((seg) => seg.id !== segmentId);
  const combinedTrack = combineSegments(segments);

  // Update start/finish checkpoints
  const checkpoints = updateStartFinishCheckpoints(
    state.route.checkpoints,
    combinedTrack,
  );

  store.update({
    route: {
      ...state.route,
      segments,
      track: combinedTrack,
      checkpoints,
    },
  });
}

// fixme this is broken
function setupSegmentDragAndDrop() {
  const items = document.querySelectorAll(".segment-item");

  items.forEach((item) => {
    // fixme: implement this for desktop + mobile. we want to be able to reorder the segments
  });

  // Delete buttons
  document.querySelectorAll(".delete-segment").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      e.stopPropagation();
      deleteSegment(e.target.dataset.id);
    });
  });
}

function addCheckpointAt(coord, km) {
  const state = store.get();
  const id = generateId();

  // Count intermediate checkpoints (excluding start/finish)
  const intermediateCount = state.route.checkpoints.filter(
    (cp) => cp.id !== CHECKPOINT_IDS.START && cp.id !== CHECKPOINT_IDS.FINISH,
  ).length;

  const newCheckpoint = {
    id,
    name: `CP${intermediateCount + 1}`,
    km,
    coord,
    // Initialize with current time as default cutoff
    cutoff: new Date().toISOString().slice(0, 16) + ":00",
  };

  // Insert before finish checkpoint
  const checkpoints = [...state.route.checkpoints];
  const finishIndex = checkpoints.findIndex((cp) => cp.id === "finish");

  if (finishIndex >= 0) {
    checkpoints.splice(finishIndex, 0, newCheckpoint);
  } else {
    checkpoints.push(newCheckpoint);
  }

  store.update({
    route: {
      ...state.route,
      checkpoints,
    },
  });

  // Setup event listeners after render
  // fixme: why do we need to call this repeatedly? Can we setup the listeners once?
  setTimeout(() => setupCheckpointListeners(), 0);
}

function addCheckpointAtClick(coord, type) {
  const state = store.get();

  if (type === "start") {
    const newCheckpoint = {
      id: "start",
      name: "Start",
      km: 0,
      coord,
      cutoff: new Date().toISOString().slice(0, 16) + ":00",
    };

    store.update({
      route: {
        ...state.route,
        checkpoints: [newCheckpoint, ...state.route.checkpoints],
      },
    });
  } else if (type === "finish") {
    const newCheckpoint = {
      id: "finish",
      name: "Finish",
      km: 0,
      coord,
    };

    store.update({
      route: {
        ...state.route,
        checkpoints: [...state.route.checkpoints, newCheckpoint],
      },
    });
  } else {
    // Intermediate checkpoint
    addCheckpointAt(coord, 0);
  }

  setTimeout(() => setupCheckpointListeners(), 0);
}

function setupCheckpointListeners() {
  // Name changes
  document.querySelectorAll(".cp-name").forEach((input) => {
    input.addEventListener("change", (e) => {
      updateCheckpoint(e.target.dataset.id, { name: e.target.value });
    });
  });

  // KM changes
  document.querySelectorAll(".cp-km").forEach((input) => {
    input.addEventListener("change", (e) => {
      const km = parseFloat(e.target.value);
      const state = store.get();

      if (state.route.track.length > 0) {
        // With track - calculate coordinate from km
        const coord = getCoordAtKm(state.route.track, km);
        updateCheckpoint(e.target.dataset.id, { km, coord: coord || [0, 0] });
      } else {
        // No track - just update km, keep existing coordinate
        updateCheckpoint(e.target.dataset.id, { km });
      }
    });
  });

  // Cutoff type selection
  document.querySelectorAll(".cp-cutoff-type").forEach((select) => {
    select.addEventListener("change", (e) => {
      const type = e.target.value;
      if (type === "absolute") {
        updateCheckpoint(e.target.dataset.id, {
          cutoff: new Date().toISOString().slice(0, 16),
        });
      } else if (type === "relative") {
        updateCheckpoint(e.target.dataset.id, { cutoffHours: 0 });
      }
    });
  });

  // Cutoff datetime changes
  document.querySelectorAll(".cp-cutoff").forEach((input) => {
    input.addEventListener("change", (e) => {
      updateCheckpoint(e.target.dataset.id, { cutoff: e.target.value + ":00" });
    });
  });

  // Cutoff hours changes
  document.querySelectorAll(".cp-hours").forEach((input) => {
    input.addEventListener("change", (e) => {
      updateCheckpoint(e.target.dataset.id, {
        cutoffHours: parseFloat(e.target.value),
      });
    });
  });

  // Delete buttons
  document.querySelectorAll(".delete-cp").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      deleteCheckpoint(e.target.dataset.id);
    });
  });

  // Setup drag and drop for reordering
  setupCheckpointDragAndDrop();
}

function setupCheckpointDragAndDrop() {
  // fixme: implement this, should be able to reorder checkpoints
}

function updateCheckpoint(id, updates) {
  const state = store.get();
  const checkpoints = state.route.checkpoints.map((cp) =>
    cp.id === id ? { ...cp, ...updates } : cp,
  );

  store.update({
    route: {
      ...state.route,
      checkpoints,
    },
  });
}

function deleteCheckpoint(id) {
  // Don't allow deleting start/finish
  if (id === "start" || id === "finish") {
    alert("Cannot delete start or finish checkpoint");
    return;
  }

  const state = store.get();
  const checkpoints = state.route.checkpoints.filter((cp) => cp.id !== id);

  store.update({
    route: {
      ...state.route,
      checkpoints,
    },
  });
}

async function saveRoute() {
  const state = store.get();

  // Validate all checkpoints have cutoff times
  const missingCutoffs = findMissingCutoffs(state.route.checkpoints);
  if (missingCutoffs.length > 0) {
    const saveBtn = document.getElementById("saveRoute");
    const saveSection = saveBtn?.parentElement;
    showInlineMessage(
      saveSection,
      `Missing cutoff times: ${missingCutoffs.map((cp) => cp.name).join(", ")}`,
      "error",
      false,
    );

    // Scroll to first missing checkpoint
    scrollToElement(".missing-cutoff");
    return;
  }

  // Don't save segments to URL, only the combined track
  const route = {
    name: state.route.name,
    created: new Date().toISOString(),
    track: state.route.track,
    checkpoints: state.route.checkpoints,
  };

  await saveRouteToURL(route);

  // Switch to tracking mode
  store.update({
    mode: "tracking",
    route: {
      ...state.route,
      created: route.created,
    },
    tracking: {
      routeId: route.created,
      arrivals: {},
    },
  });
}

// ============================================================================
// TRACKING MODE
// ============================================================================

function renderTrackingMode() {
  const state = store.get();
  const app = document.querySelector("#app");

  // fixme: we should prioritize information about the next checkpoint, eta, distance to, etc.
  // fixme: the table view is inconveniently small and requires horizontal scroll to action. should we make it a card? experiment with wider version
  app.innerHTML = `
    <div class="tracking-container">
      <h1>${state.route.name}</h1>

      <div class="tracking-header">
        <button id="getLocation">📍 Where Am I?</button>
      </div>

      <div class="summary-section">
        ${renderSummary(state)}
      </div>

      <div class="next-cutoff-section">
        ${renderNextCutoff(state)}
      </div>

      <div class="map-section">
        <button id="toggleMap" class="toggle-map-btn">
          <span id="mapToggleIcon">▼</span> Map
        </button>
        <div id="mapContainer" class="map-container" style="display: block;">
          <div id="map" style="height: 100%;"></div>
        </div>
      </div>

      <div class="checkpoints-section">
        <h2>Checkpoints</h2>
        ${renderTrackingCheckpoints(state)}
      </div>
    </div>
  `;

  // Setup checkpoint check-in listeners
  setupTrackingListeners();

  // Initialize tracking map
  initTrackingMap();

  // Auto-populate start checkpoint with event start time
  autoPopulateStartTime();
}

function autoPopulateStartTime() {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const startCp = sorted[0];

  if (!startCp) return;

  // If start checkpoint doesn't have an arrival time, set it from cutoff
  //
  // fixme: this is dump and over engineered. just set the time to the cutoff
  // time. startcp will never be using the cutoffhours
  if (!state.tracking.arrivals[startCp.id]) {
    const startTime = startCp.cutoff
      ? new Date(startCp.cutoff)
      : startCp.cutoffHours != null
        ? new Date(Date.now() + startCp.cutoffHours * 3600000)
        : null;

    if (startTime) {
      const tracking = {
        ...state.tracking,
        arrivals: {
          ...state.tracking.arrivals,
          [startCp.id]: startTime.toISOString(),
        },
      };

      store.update({ tracking });
      saveTracking(tracking);
    }
  }
}

function initTrackingMap() {
  const state = store.get();

  // Clean up existing map
  if (mapInstance) {
    mapInstance.destroy();
  }

  // Create new map
  mapInstance = createMap("map");

  // Show track if available
  if (state.route.track.length > 0) {
    mapInstance.showTrack(state.route.track, { color: "#10b981" });
  }

  // Show checkpoints
  mapInstance.showCheckpoints(state.route.checkpoints, {
    draggable: false,
  });

  // Fit map to content
  mapInstance.fitToContent();

  // Setup location button
  const locationBtn = document.getElementById("getLocation");
  if (locationBtn) {
    locationBtn.addEventListener("click", getUserLocation);
  }

  // Setup map toggle
  const toggleMapBtn = document.getElementById("toggleMap");
  const mapContainer = document.getElementById("mapContainer");
  const mapToggleIcon = document.getElementById("mapToggleIcon");

  if (toggleMapBtn && mapContainer) {
    toggleMapBtn.addEventListener("click", () => {
      const isVisible = mapContainer.style.display !== "none";
      mapContainer.style.display = isVisible ? "none" : "block";
      mapToggleIcon.textContent = isVisible ? "▶" : "▼";

      // Resize map after showing
      if (!isVisible) {
        setTimeout(() => mapInstance.resize(), 100);
      }
    });

    // Start collapsed on mobile
    if (window.innerWidth <= 768) {
      mapContainer.style.display = "none";
      mapToggleIcon.textContent = "▶";
    }
  }
}

function getUserLocation() {
  if (!navigator.geolocation) {
    alert("Geolocation is not supported by your browser");
    return;
  }

  const state = store.get();

  navigator.geolocation.getCurrentPosition(
    (position) => {
      const userCoord = [position.coords.longitude, position.coords.latitude];

      // Show user location on map
      mapInstance.showUserLocation(userCoord);

      // If we have a track, snap to it
      if (state.route.track.length > 0) {
        const snapped = snapToTrack(state.route.track, userCoord);
        if (snapped) {
          // Calculate distance to track
          const distToTrack = calculateDistance(userCoord, snapped.coord);

          mapInstance.showSnappedLocation(snapped.coord, snapped.km);

          // Draw dashed line if user is close to route (within 50km)
          if (distToTrack < 50) {
            mapInstance.drawLineToTrack(userCoord, snapped.coord);
          } else {
            mapInstance.clearLineToTrack();
          }

          // Update UI state
          store.updateNestedSilent("ui.userLocation", userCoord);
          store.updateNestedSilent("ui.snappedLocation", {
            coord: snapped.coord,
            km: snapped.km,
          });

          // Show alert with distance info
          if (distToTrack < 50) {
            const nextCp = findNextCheckpointByKm(state, snapped.km);
            if (nextCp) {
              const distToNext = nextCp.km - snapped.km;
              const mapSection = document.querySelector(".map-section");
              showInlineMessage(
                mapSection,
                `You are at ${snapped.km.toFixed(1)} km. Next checkpoint: ${nextCp.name} (${distToNext.toFixed(1)} km away)`,
                "success",
              );
            }
          } else {
            const mapSection = document.querySelector(".map-section");
            showInlineMessage(
              mapSection,
              `You are ${distToTrack.toFixed(1)} km from the route. You're not racing yet are you?`,
              "info",
            );
          }
        }
      }
    },
    (error) => {
      const mapSection = document.querySelector(".map-section");
      showInlineMessage(
        mapSection,
        `Unable to get location: ${error.message}`,
        "error",
      );
    },
  );
}

function findNextCheckpointByKm(state, currentKm) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  return sorted.find((cp) => cp.km > currentKm);
}

function renderNextCutoff(state) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const { startTime } = getStartInfo(state);

  // Find next uncompleted checkpoint
  const nextCp =
    findNextCheckpoint(sorted, state.tracking) || sorted[sorted.length - 1];

  const cutoffTime = getCutoffTimeForCheckpoint(nextCp, startTime);

  if (!cutoffTime) {
    return '<div class="next-cutoff-card"><p class="hint">No cutoff time set for next checkpoint</p></div>';
  }

  const now = new Date();
  const isFuture = startTime && now.getTime() < startTime.getTime();
  const { days, hours, minutes, isPast } = calculateTimeRemaining(cutoffTime);

  let statusMessage = "";
  let statusClass = "";

  if (isFuture) {
    const { days: daysToStart, hours: hoursToStart } =
      calculateTimeRemaining(startTime);
    statusMessage = `Event starts in ${daysToStart}d ${hoursToStart}h`;
    statusClass = "future";
  } else if (isPast) {
    statusMessage = days > 30 ? "Event ended" : `Cutoff passed ${days}d ago`;
    statusClass = "past";
  } else {
    statusMessage = `${formatTimeRemaining({ days, hours, minutes })} remaining`;
    const threeHours = 3 * 60 * 60 * 1000;
    statusClass =
      cutoffTime.getTime() - now.getTime() < threeHours ? "urgent" : "active";
  }

  return `
    <div class="next-cutoff-card ${statusClass}">
      <h3>Next Cutoff: ${nextCp.name}</h3>
      <p class="cutoff-time">${formatDateTime(cutoffTime)}</p>
      <p class="time-remaining">${statusMessage}</p>
    </div>
  `;
}

function renderSummary(state) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const { startTime } = getStartInfo(state);

  const stats = calculateSummaryStats(state.tracking, sorted, startTime);

  // fixme: these aren't used?
  const totalKm =
    state.route.track.length > 0
      ? trackLength(state.route.track)
      : sorted[sorted.length - 1]?.km || 0;
  const completedCount = Object.keys(state.tracking.arrivals).length;
  const totalCount = state.route.checkpoints.length;

  return `
    <div class="summary-cards">
      <div class="summary-card">
        <h3>Overall Pace</h3>
        <div class="value">${stats.overallPace > 0 ? stats.overallPace.toFixed(1) : "--"} km/h</div>
      </div>
      <div class="summary-card">
        <h3>Distance Covered</h3>
        <div class="value">${stats.distanceCovered > 0 ? stats.distanceCovered.toFixed(1) : "0"} km</div>
      </div>
      <div class="summary-card">
        <h3>Time vs Schedule</h3>
        <div class="value">${stats.timeAheadBehindStr} <b>${stats.timeAheadBehind > 0 ? "ahead" : stats.timeAheadBehind < 0 ? "behind" : ""}</b></div>
      </div>
      <div class="summary-card">
        <h3>Finish ETA</h3>
        <div class="value">${formatDateTime(stats.estimatedFinish)}</div>
      </div>
    </div>
  `;
}

function renderTrackingCheckpoints(state) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const { startTime } = getStartInfo(state);
  const currentIndex = getCurrentCheckpointIndex(sorted, state.tracking);

  return `
    <table class="tracking-table">
      <thead>
        <tr>
          <th>Checkpoint</th>
          <th><span class="full-label">Distance</span><span class="short-label">Dist</span></th>
          <th>Cutoff</th>
          <th><span class="full-label">Arrival / ETA</span><span class="short-label">Time</span></th>
          <th><span class="full-label">Recorded Pace</span><span class="short-label">Rec</span></th>
          <th><span class="full-label">Minimum Pace</span><span class="short-label">Min</span></th>
          <th><span class="full-label">Time Ahead/Behind</span><span class="short-label">+/-</span></th>
          <th><span class="full-label">Distance From Last</span><span class="short-label">Δ</span></th>
          <th></th>
        </tr>
      </thead>
      <tbody>
        ${sorted
          .map((cp, index) => {
            // Calculate all metrics using helper
            const metrics = calculateCheckpointMetrics(
              cp,
              index,
              sorted,
              state,
            );
            const {
              hasArrived,
              arrival,
              distFromLast,
              currentPace,
              requiredPace,
              timeAheadBehind,
              estimatedArrival,
              remainingTimeStr,
            } = metrics;

            // Can only check in if previous checkpoint is complete
            const isStart = cp.id === "start";
            const canCheckInNow = canCheckIn(cp, index, sorted, state);

            return `
            <tr class="${hasArrived ? "reached" : "upcoming"}">
              <td><strong>${cp.name}</strong></td>
              <td>${cp.km.toFixed(1)} km</td>
              <td>${formatCutoff(cp, state)}${remainingTimeStr ? `<br/><em>${remainingTimeStr}</em>` : ""}</td>
              <td>${hasArrived ? `<span class="arrival-time" data-id="${cp.id}">${formatDateTime(new Date(arrival))}</span> <button class="edit-arrival" data-id="${cp.id}">✏️</button>` : estimatedArrival ? `<b>ETA:</b> ${formatDateTime(estimatedArrival)}` : "-"}</td>
              <td class="${currentPace ? (currentPace >= 10 ? "metric-positive" : "metric-negative") : "metric-neutral"}">
                ${index === 0 ? "n/a" : currentPace ? currentPace.toFixed(1) + " km/h" : "-"}
              </td>
              <td class="${requiredPace ? (requiredPace <= 20 && requiredPace > 0 ? "metric-positive" : "metric-negative") : "metric-neutral"}">
                ${index === 0 ? "n/a" : requiredPace ? requiredPace.toFixed(1) + " km/h" : "-"}
              </td>
              <td class="${timeAheadBehind !== null ? (timeAheadBehind > 0 ? "metric-positive" : "metric-negative") : "metric-neutral"}">
                ${index === 0 ? "n/a" : timeAheadBehind !== null ? (timeAheadBehind > 0 ? "+" : "") + timeAheadBehind.toFixed(1) + "h" : "-"}
              </td>
              <td>${distFromLast} km</td>
              <td>
                ${
                  hasArrived
                    ? isStart
                      ? `<span class="auto-label">Auto</span>`
                      : `<button class="clear-arrival" data-id="${cp.id}">Clear</button>`
                    : canCheckInNow
                      ? `<button class="check-in" data-id="${cp.id}">Check In</button>`
                      : `<button class="check-in" disabled>Check In</button>`
                }
              </td>
            </tr>
          `;
          })
          .join("")}
      </tbody>
    </table>
  `;
}

function formatCutoff(checkpoint, state) {
  return formatCutoffTime(checkpoint, state);
}

function setupTrackingListeners() {
  document.querySelectorAll(".check-in").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      checkInAt(e.target.dataset.id);
    });
  });

  document.querySelectorAll(".clear-arrival").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      clearArrival(e.target.dataset.id);
    });
  });

  document.querySelectorAll(".edit-arrival").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      editArrival(e.target.dataset.id);
    });
  });
}

function checkInAt(checkpointId) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);

  // Verify all previous checkpoints are checked in
  const validation = validateSequentialCheckIn(
    checkpointId,
    sorted,
    state.tracking,
  );
  // fixme: this is dumb. just disable the buttons if it's not valid and don't let user click
  if (!validation.isValid) {
    const checkpointsSection = document.querySelector(".checkpoints-section");
    const msg = showInlineMessage(
      checkpointsSection,
      `Please check in at ${validation.missingCheckpoint.name} first.`,
      "error",
    );
    if (msg) {
      checkpointsSection.insertBefore(msg, checkpointsSection.firstChild);
    }

    // Scroll to the checkpoint that needs to be checked in
    scrollToElement(`tr[class*="upcoming"]`);
    return;
  }

  const now = new Date().toISOString();

  const tracking = {
    ...state.tracking,
    arrivals: {
      ...state.tracking.arrivals,
      [checkpointId]: now,
    },
  };

  store.update({ tracking });
  saveTracking(tracking);
}

function clearArrival(checkpointId) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);

  // Validate that checkpoint can be cleared
  // fixme: wtf is this
  const validation = validateClearCheckpoint(checkpointId, sorted);
  if (!validation.canClear) {
    const checkpointsSection = document.querySelector(".checkpoints-section");
    const msg = showInlineMessage(
      checkpointsSection,
      validation.reason,
      "error",
    );
    if (msg) {
      checkpointsSection.insertBefore(msg, checkpointsSection.firstChild);
    }
    return;
  }

  const { [checkpointId]: removed, ...rest } = state.tracking.arrivals;

  const tracking = {
    ...state.tracking,
    arrivals: rest,
  };

  store.update({ tracking });
  saveTracking(tracking);
}

function editArrival(checkpointId) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const checkpoint = sorted.find((cp) => cp.id === checkpointId);

  if (!checkpoint) return;

  const currentTime = state.tracking.arrivals[checkpointId];
  const currentDate = currentTime ? new Date(currentTime) : new Date();
  const formatted = formatDateTimeLocal(currentDate);

  // Create modal dialog
  const modal = createModal(
    `
      <h3>Edit Arrival Time</h3>
      <p>${checkpoint.name}</p>
      <input type="datetime-local" id="editTimeInput" value="${formatted}" />
      <div class="modal-buttons">
        <button id="saveEditTime" class="primary">Save</button>
        <button id="cancelEditTime">Cancel</button>
      </div>
    `,
    null,
  );

  document.body.appendChild(modal);

  // Focus input
  setTimeout(() => {
    document.getElementById("editTimeInput")?.focus();
  }, 100);

  // Handle save
  document.getElementById("saveEditTime")?.addEventListener("click", () => {
    const input = document.getElementById("editTimeInput");
    const newTime = input?.value;

    if (newTime) {
      const newDate = new Date(newTime);
      if (!isNaN(newDate.getTime())) {
        const tracking = {
          ...state.tracking,
          arrivals: {
            ...state.tracking.arrivals,
            [checkpointId]: newDate.toISOString(),
          },
        };

        store.update({ tracking });
        saveTracking(tracking);
        modal.remove();
      } else {
        const checkpointsSection = document.querySelector(
          ".checkpoints-section",
        );
        showInlineMessage(checkpointsSection, "Invalid date format", "error");
      }
    }
  });

  // Handle cancel
  document.getElementById("cancelEditTime")?.addEventListener("click", () => {
    modal.remove();
  });
}

// Start the app
//
// fixme: let's split out files for the "setup" stage and the "tracking" stage.
// any shared UI components can be split
init();
