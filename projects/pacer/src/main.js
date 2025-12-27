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

// Create the app store
const store = createStore(createInitialState());

// Map instance (created on demand)
let mapInstance = null;

// Helper function to show inline messages
function showInlineMessage(
  container,
  message,
  type = "error",
  autoDismiss = true,
) {
  if (!container) return;

  // Remove existing message
  const existing = container.querySelector(".inline-message");
  if (existing) existing.remove();

  const msgEl = document.createElement("p");
  msgEl.className = `inline-message ${type}`;
  msgEl.style.fontWeight = "500";
  msgEl.style.marginTop = "0.5rem";
  msgEl.style.marginBottom = "0.5rem";
  msgEl.style.padding = "0.5rem";
  msgEl.style.borderRadius = "4px";
  msgEl.textContent = message;

  if (type === "error") {
    msgEl.style.color = "#dc2626";
    msgEl.style.backgroundColor = "#fef2f2";
  } else if (type === "success") {
    msgEl.style.color = "#059669";
    msgEl.style.backgroundColor = "#f0fdf4";
  } else if (type === "info") {
    msgEl.style.color = "#6b7280";
    msgEl.style.backgroundColor = "#f9fafb";
  }

  container.appendChild(msgEl);

  if (autoDismiss) {
    setTimeout(() => msgEl.remove(), 5000);
  }

  return msgEl;
}

// Helper function to ensure start/finish checkpoints exist
function updateStartFinishCheckpoints(existingCheckpoints, track) {
  const checkpoints = [...existingCheckpoints];

  // Find existing start/finish
  let startCp = checkpoints.find((cp) => cp.id === "start");
  let finishCp = checkpoints.find((cp) => cp.id === "finish");

  if (track.length > 0) {
    const totalLength = trackLength(track);

    // Update or create start checkpoint
    if (startCp) {
      startCp.km = 0;
      startCp.coord = track[0];
    } else {
      startCp = {
        id: "start",
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
      finishCp = {
        id: "finish",
        name: "Finish",
        km: totalLength,
        coord: track[track.length - 1],
        // Default cutoff time (can be edited)
      };
      checkpoints.push(finishCp);
    }
  } else {
    // No track - create start/finish with manual entry if they don't exist
    // Initialize near Berlin so they're visible on the default map view
    if (!startCp) {
      startCp = {
        id: "start",
        name: "Start",
        km: 0,
        coord: [13.38, 52.52], // Berlin area - west
        // Default to current time as placeholder for event start
        cutoff: new Date().toISOString().slice(0, 16) + ":00",
      };
      checkpoints.unshift(startCp);
    }

    if (!finishCp) {
      finishCp = {
        id: "finish",
        name: "Finish",
        km: 0,
        coord: [13.43, 52.52], // Berlin area - east
        // Default cutoff time (can be edited)
      };
      checkpoints.push(finishCp);
    }
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
    // Initialize with start/finish checkpoints
    const initialState = store.get();
    const checkpoints = updateStartFinishCheckpoints([], []);

    store.update({
      mode: "setup",
      route: {
        ...initialState.route,
        checkpoints,
      },
    });
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
      // Segments changed - update segments list
      if (state.route.segments !== lastState.route.segments) {
        const segmentsList = document.getElementById("segmentsList");
        if (segmentsList) {
          segmentsList.innerHTML = renderSegmentsList(state);
          setupSegmentDragAndDrop();
        }

        // Update track length display
        const successMsg =
          state.route.track.length > 0
            ? `<p class="success">✓ Combined track: ${trackLength(state.route.track).toFixed(1)} km (${state.route.track.length} points)</p>`
            : "";
        const trackSection = segmentsList?.parentElement;
        if (trackSection) {
          const existing = trackSection.querySelector(".success");
          if (existing && !successMsg) {
            existing.remove();
          } else if (!existing && successMsg) {
            const temp = document.createElement("div");
            temp.innerHTML = successMsg;
            trackSection.appendChild(temp.firstChild);
          }
        }

        // Update map
        if (mapInstance) {
          mapInstance.clearTrack();
          if (state.route.track.length > 0) {
            mapInstance.showTrack(state.route.track);
          }
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
          mapInstance.fitToContent();
        }
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
          const existingHint = saveSection.querySelector(
            '.hint[style*="dc2626"]',
          );
          if (!canSaveRoute(state)) {
            if (!existingHint) {
              const hint = document.createElement("p");
              hint.className = "hint";
              hint.style.color = "#dc2626";
              hint.textContent = "All checkpoints must have cutoff times";
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

  app.innerHTML = `
    <div class="setup-container">
      <h1>Pacer - Setup Route</h1>

      <div class="setup-layout">
        <div class="setup-form">
          <div class="setup-section">
            <input type="text" id="routeName" value="${state.route.name}" placeholder="Route name (e.g. TCR 2025)" style="font-size: 1.1em; font-weight: 500;">

            <h3>Track & Checkpoints</h3>

            <input type="file" id="gpxFiles" accept=".gpx" multiple />
            ${
              state.route.track.length > 0
                ? `<p class="success">✓ ${trackLength(state.route.track).toFixed(0)} km track (${state.route.segments.length} segment${state.route.segments.length !== 1 ? "s" : ""})</p>`
                : '<p class="hint">Upload GPX file(s) or position start/finish manually</p>'
            }

            <div id="segmentsList">
              ${renderSegmentsList(state)}
            </div>

            <p class="hint" style="color: #dc2626; font-weight: 500; margin-top: 0.5rem;">
              ⚠ All checkpoints require cutoff times. Start = event start time.
            </p>
            <div id="checkpointsList">
              ${renderCheckpointsList(state)}
            </div>

            <button id="saveRoute" class="primary" ${canSaveRoute(state) ? "" : "disabled"} style="width: 100%; margin-top: 1rem;">
              Save & Generate URL
            </button>
            ${!canSaveRoute(state) ? '<p class="hint" style="color: #dc2626;">All checkpoints must have cutoff times</p>' : ""}
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
  mapInstance.onMapClick((coord) => {
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
      setTimeout(() => {
        document
          .getElementById("confirmAddCheckpoint")
          ?.addEventListener("click", () => {
            addCheckpointAt(snapped.coord, snapped.km);
            mapInstance.getMap().closePopup();
          });
      }, 0);
    } else {
      // No track - show popup to add checkpoint at clicked location
      const popup = L.popup()
        .setLatLng([coord[1], coord[0]])
        .setContent(
          `
          <div style="text-align: center;">
            <p style="margin: 0 0 8px 0;"><strong>Add checkpoint here?</strong></p>
            <button id="confirmAddCheckpoint" style="padding: 4px 12px; cursor: pointer;">Add Checkpoint</button>
          </div>
        `,
        )
        .openOn(mapInstance.getMap());

      setTimeout(() => {
        document
          .getElementById("confirmAddCheckpoint")
          ?.addEventListener("click", () => {
            addCheckpointAt(coord, 0);
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
            const isStartOrFinish = cp.id === "start" || cp.id === "finish";
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

function canSaveRoute(state) {
  // Check if route has name
  if (!state.route.name) return false;

  // Check if we have at least 2 checkpoints (start + finish)
  if (state.route.checkpoints.length < 2) return false;

  // Check that all checkpoints have cutoff times
  return state.route.checkpoints.every((cp) => {
    return cp.cutoff || cp.cutoffHours != null;
  });
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
  const combinedTrack = combineSegments(allSegments);

  // Update or create start/finish checkpoints
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
  setTimeout(() => setupSegmentDragAndDrop(), 0);
}

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

function setupSegmentDragAndDrop() {
  let draggedElement = null;
  let draggedId = null;

  const items = document.querySelectorAll(".segment-item");

  items.forEach((item) => {
    item.addEventListener("dragstart", (e) => {
      draggedElement = item;
      draggedId = item.dataset.id;
      item.classList.add("dragging");
    });

    item.addEventListener("dragend", (e) => {
      item.classList.remove("dragging");
      draggedElement = null;
      draggedId = null;
    });

    item.addEventListener("dragover", (e) => {
      e.preventDefault();
      const afterElement = getDragAfterElement(item.parentElement, e.clientY);
      const dragging = document.querySelector(".dragging");

      if (afterElement == null) {
        item.parentElement.appendChild(dragging);
      } else {
        item.parentElement.insertBefore(dragging, afterElement);
      }
    });

    item.addEventListener("drop", (e) => {
      e.preventDefault();
      reorderSegments();
    });
  });

  // Delete buttons
  document.querySelectorAll(".delete-segment").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      e.stopPropagation();
      deleteSegment(e.target.dataset.id);
    });
  });
}

function getDragAfterElement(container, y) {
  const draggableElements = [
    ...container.querySelectorAll(".segment-item:not(.dragging)"),
  ];

  return draggableElements.reduce(
    (closest, child) => {
      const box = child.getBoundingClientRect();
      const offset = y - box.top - box.height / 2;

      if (offset < 0 && offset > closest.offset) {
        return { offset: offset, element: child };
      } else {
        return closest;
      }
    },
    { offset: Number.NEGATIVE_INFINITY },
  ).element;
}

function reorderSegments() {
  const items = document.querySelectorAll(".segment-item");
  const newOrder = Array.from(items).map((item) => item.dataset.id);

  const state = store.get();
  const reorderedSegments = newOrder.map((id) =>
    state.route.segments.find((seg) => seg.id === id),
  );

  const combinedTrack = combineSegments(reorderedSegments);

  // Update start/finish checkpoints for new track order
  const checkpoints = updateStartFinishCheckpoints(
    state.route.checkpoints,
    combinedTrack,
  );

  store.update({
    route: {
      ...state.route,
      segments: reorderedSegments,
      track: combinedTrack,
      checkpoints,
    },
  });
}

function addCheckpointAt(coord, km) {
  const state = store.get();
  const id = generateId();

  // Count intermediate checkpoints (excluding start/finish)
  const intermediateCount = state.route.checkpoints.filter(
    (cp) => cp.id !== "start" && cp.id !== "finish",
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
  const tbody = document.getElementById("checkpointsTableBody");
  if (!tbody) return;

  let draggedRow = null;

  tbody.querySelectorAll("tr[draggable='true']").forEach((row) => {
    row.addEventListener("dragstart", (e) => {
      draggedRow = row;
      row.classList.add("dragging");
    });

    row.addEventListener("dragend", (e) => {
      row.classList.remove("dragging");
      draggedRow = null;
    });

    row.addEventListener("dragover", (e) => {
      e.preventDefault();
      const afterElement = getDragAfterElementRow(tbody, e.clientY);
      const dragging = tbody.querySelector(".dragging");

      if (afterElement == null) {
        tbody.appendChild(dragging);
      } else {
        tbody.insertBefore(dragging, afterElement);
      }
    });

    row.addEventListener("drop", (e) => {
      e.preventDefault();
      reorderCheckpoints();
    });
  });
}

function getDragAfterElementRow(container, y) {
  const draggableElements = [
    ...container.querySelectorAll("tr[draggable='true']:not(.dragging)"),
  ];

  return draggableElements.reduce(
    (closest, child) => {
      const box = child.getBoundingClientRect();
      const offset = y - box.top - box.height / 2;

      if (offset < 0 && offset > closest.offset) {
        return { offset: offset, element: child };
      } else {
        return closest;
      }
    },
    { offset: Number.NEGATIVE_INFINITY },
  ).element;
}

function reorderCheckpoints() {
  const tbody = document.getElementById("checkpointsTableBody");
  const rows = Array.from(tbody.querySelectorAll("tr"));
  const newOrder = rows.map((row) => row.dataset.id);

  const state = store.get();

  // Preserve start/finish at beginning and end
  const startCp = state.route.checkpoints.find((cp) => cp.id === "start");
  const finishCp = state.route.checkpoints.find((cp) => cp.id === "finish");

  // Get intermediate checkpoints in new order
  const intermediate = newOrder
    .filter((id) => id !== "start" && id !== "finish")
    .map((id) => state.route.checkpoints.find((cp) => cp.id === id))
    .filter(Boolean);

  const reorderedCheckpoints = [startCp, ...intermediate, finishCp].filter(
    Boolean,
  );

  store.update({
    route: {
      ...state.route,
      checkpoints: reorderedCheckpoints,
    },
  });
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
  const missingCutoffs = state.route.checkpoints.filter(
    (cp) => !cp.cutoff && cp.cutoffHours == null,
  );
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
    const firstMissing = document.querySelector(".missing-cutoff");
    if (firstMissing) {
      firstMissing.scrollIntoView({ behavior: "smooth", block: "center" });
    }
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
            const nextCp = findNextCheckpoint(state, snapped.km);
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

function findNextCheckpoint(state, currentKm) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  return sorted.find((cp) => cp.km > currentKm);
}

function renderNextCutoff(state) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const startCp = sorted[0];
  const startTime = state.tracking.arrivals[startCp?.id]
    ? new Date(state.tracking.arrivals[startCp.id])
    : startCp?.cutoff
      ? new Date(startCp.cutoff)
      : null;

  // Find next uncompleted checkpoint
  let nextCp = null;
  for (const cp of sorted) {
    if (!state.tracking.arrivals[cp.id]) {
      nextCp = cp;
      break;
    }
  }

  // If all checkpoints complete, show finish
  if (!nextCp) {
    nextCp = sorted[sorted.length - 1];
  }

  const now = new Date();
  const cutoffTime = getCutoffTime(nextCp, startTime);

  if (!cutoffTime) {
    return '<div class="next-cutoff-card"><p class="hint">No cutoff time set for next checkpoint</p></div>';
  }

  const timeRemaining = cutoffTime.getTime() - now.getTime();
  const isPast = timeRemaining < 0;
  const isFuture = startTime && now.getTime() < startTime.getTime();

  let statusMessage = "";
  let statusClass = "";

  if (isFuture) {
    const timeToStart = startTime.getTime() - now.getTime();
    const daysToStart = Math.floor(timeToStart / (1000 * 60 * 60 * 24));
    const hoursToStart = Math.floor(
      (timeToStart % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60),
    );
    statusMessage = `Event starts in ${daysToStart}d ${hoursToStart}h`;
    statusClass = "future";
  } else if (isPast) {
    const timePast = Math.abs(timeRemaining);
    const daysPast = Math.floor(timePast / (1000 * 60 * 60 * 24));
    statusMessage =
      daysPast > 30 ? "Event ended" : `Cutoff passed ${daysPast}d ago`;
    statusClass = "past";
  } else {
    const days = Math.floor(timeRemaining / (1000 * 60 * 60 * 24));
    const hours = Math.floor(
      (timeRemaining % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60),
    );
    const minutes = Math.floor(
      (timeRemaining % (1000 * 60 * 60)) / (1000 * 60),
    );
    statusMessage = `${days}d ${hours}h ${minutes}m remaining`;
    statusClass = timeRemaining < 3 * 60 * 60 * 1000 ? "urgent" : "active"; // < 3 hours = urgent
  }

  return `
    <div class="next-cutoff-card ${statusClass}">
      <h3>Next Cutoff: ${nextCp.name}</h3>
      <p class="cutoff-time">${formatDateTime(cutoffTime)}</p>
      <p class="time-remaining">${statusMessage}</p>
    </div>
  `;
}

function getCutoffTime(checkpoint, startTime) {
  if (checkpoint.cutoff) {
    return new Date(checkpoint.cutoff);
  }
  if (checkpoint.cutoffHours != null && startTime) {
    return new Date(startTime.getTime() + checkpoint.cutoffHours * 3600000);
  }
  return null;
}

function renderSummary(state) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const startCp = sorted[0];
  const startTime = state.tracking.arrivals[startCp?.id]
    ? new Date(state.tracking.arrivals[startCp.id])
    : null;

  const stats = calculateSummaryStats(state.tracking, sorted, startTime);

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
  const startCp = sorted[0];
  const startTime = state.tracking.arrivals[startCp?.id]
    ? new Date(state.tracking.arrivals[startCp.id])
    : null;

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
            const arrival = state.tracking.arrivals[cp.id];
            const hasArrived = !!arrival;
            const prevCp = index > 0 ? sorted[index - 1] : null;

            // Calculate distance from last checkpoint
            const distFromLast = prevCp
              ? (cp.km - prevCp.km).toFixed(1)
              : "0.0";

            // Calculate paces
            let currentPace = null;
            let requiredPace = null;
            let timeAheadBehind = null;
            let estimatedArrival = null;
            let remainingTimeStr = null;

            // Don't show pace calculations for start checkpoint
            if (index > 0) {
              if (hasArrived && prevCp) {
                currentPace = calculateCurrentPace(state.tracking, prevCp, cp);
              }

              if (currentIndex < index && startTime) {
                const fromCp = sorted[currentIndex];
                requiredPace = calculateRequiredPace(
                  state.tracking,
                  fromCp,
                  cp,
                  startTime,
                );
              }

              if (hasArrived && startTime) {
                timeAheadBehind = calculateTimeAheadBehind(
                  state.tracking,
                  cp,
                  startTime,
                );
              }
            }

            if (!hasArrived && index > currentIndex && startTime) {
              estimatedArrival = calculateEstimatedArrival(
                state.tracking,
                sorted,
                cp,
              );
              const cutoffTime = cp.cutoff
                ? new Date(cp.cutoff)
                : cp.cutoffHours != null && startTime
                  ? new Date(startTime.getTime() + cp.cutoffHours * 3600000)
                  : null;
              if (cutoffTime) {
                remainingTimeStr = formatTimeDifference(cutoffTime, new Date());
              }
            }

            // Can only check in if previous checkpoint is complete
            // For start checkpoint, don't allow checking in (use cutoff time as start time)
            const isStart = cp.id === "start";
            const canCheckIn =
              !hasArrived &&
              !isStart &&
              (index === 0 || state.tracking.arrivals[sorted[index - 1].id]);

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
                    : canCheckIn
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
  if (checkpoint.cutoff) {
    return formatDateTime(new Date(checkpoint.cutoff));
  }
  if (checkpoint.cutoffHours != null) {
    const startCp = state.route.checkpoints[0];
    const startTime = state.tracking.arrivals[startCp?.id];
    if (startTime) {
      const cutoffTime = new Date(
        new Date(startTime).getTime() + checkpoint.cutoffHours * 3600000,
      );
      return (
        formatDateTime(cutoffTime) + ` (${checkpoint.cutoffHours}h from start)`
      );
    }
    return `${checkpoint.cutoffHours}h from start`;
  }
  return "-";
}

function formatDateTime(date) {
  if (!date || isNaN(date.getTime())) return "-";

  const options = {
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  };
  return date.toLocaleString("en-US", options);
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
  const cpIndex = sorted.findIndex((cp) => cp.id === checkpointId);

  // Verify all previous checkpoints are checked in
  if (cpIndex > 0) {
    for (let i = 0; i < cpIndex; i++) {
      if (!state.tracking.arrivals[sorted[i].id]) {
        const checkpointsSection = document.querySelector(
          ".checkpoints-section",
        );
        const msg = showInlineMessage(
          checkpointsSection,
          `Please check in at ${sorted[i].name} first.`,
          "error",
        );
        if (msg) {
          checkpointsSection.insertBefore(msg, checkpointsSection.firstChild);
        }

        // Scroll to the checkpoint that needs to be checked in
        const targetRow = document.querySelector(`tr[class*="upcoming"]`);
        if (targetRow) {
          targetRow.scrollIntoView({ behavior: "smooth", block: "center" });
        }
        return;
      }
    }
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
  const startCp = sorted[0];

  // Don't allow clearing start checkpoint
  if (checkpointId === startCp.id) {
    const checkpointsSection = document.querySelector(".checkpoints-section");
    const msg = showInlineMessage(
      checkpointsSection,
      "Cannot clear start time. This is set from the event start time.",
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

  // Allow editing start checkpoint (it's the event start time, user may need to correct it)

  const currentTime = state.tracking.arrivals[checkpointId];
  const currentDate = currentTime ? new Date(currentTime) : new Date();

  // Format for datetime-local input
  const year = currentDate.getFullYear();
  const month = String(currentDate.getMonth() + 1).padStart(2, "0");
  const day = String(currentDate.getDate()).padStart(2, "0");
  const hours = String(currentDate.getHours()).padStart(2, "0");
  const minutes = String(currentDate.getMinutes()).padStart(2, "0");
  const formatted = `${year}-${month}-${day}T${hours}:${minutes}`;

  // Create modal dialog
  const modal = document.createElement("div");
  modal.className = "edit-time-modal";
  modal.innerHTML = `
    <div class="edit-time-modal-content">
      <h3>Edit Arrival Time</h3>
      <p>${checkpoint.name}</p>
      <input type="datetime-local" id="editTimeInput" value="${formatted}" />
      <div class="modal-buttons">
        <button id="saveEditTime" class="primary">Save</button>
        <button id="cancelEditTime">Cancel</button>
      </div>
    </div>
  `;

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

  // Handle click outside
  modal.addEventListener("click", (e) => {
    if (e.target === modal) {
      modal.remove();
    }
  });

  // Handle escape key
  const escapeHandler = (e) => {
    if (e.key === "Escape") {
      modal.remove();
      document.removeEventListener("keydown", escapeHandler);
    }
  };
  document.addEventListener("keydown", escapeHandler);
}

// Start the app
init();
