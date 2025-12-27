/**
 * Setup Mode Module
 * Contains all functions specific to route setup functionality
 */

// Import shared dependencies
import { createStore, generateId, getCutoffTime, findCheckpoint, getStartCheckpoint, getStartTime } from "../../shared/state.js";
import { parseGPX, simplifyTrack, calculateTrackLength, getCoordAtKm, sortCheckpointsByDistance, snapToTrack, findCheckpointOnTrack, calculateDistance, distanceBetweenCheckpoints } from "../../shared/geo.js";
import { showInlineMessage, formatDateTime, formatCutoffTime, getStartInfo, formatDateTimeLocal, scrollToElement, canSaveRoute, findMissingCutoffs, createModal, calculateTimeRemaining, formatTimeRemaining } from "../../shared/ui.js";
import { calculateCheckpointMetrics, canCheckIn, findNextCheckpoint, getCutoffTimeForCheckpoint, validateClearCheckpoint, validateSequentialCheckIn } from "../../shared/checkpoint-ops.js";
import { saveRouteToURL, loadRouteFromURL, saveTracking, loadTracking } from "../../shared/storage.js";
import { createMap } from "../../shared/map.js";
import L from "leaflet";
import { calculateCurrentPace, calculateRequiredPace, calculateEstimatedArrival, calculateTimeAheadBehind, calculateSummaryStats, getCurrentCheckpointIndex, formatTimeDifference, getTimeDifferenceHours } from "../../shared/pace.js";

// Constants
const CHECKPOINT_IDS = {
  START: "start",
  FINISH: "finish",
};

// Map instance
let mapInstance = null;

// Helper function to ensure start/finish checkpoints exist
function updateStartFinishCheckpoints(existingCheckpoints, track) {
  const checkpoints = [...existingCheckpoints];

  // Only create/update start/finish if we have a track
  if (track.length === 0) {
    return checkpoints;
  }

  // Find existing start/finish
  let startCp = checkpoints.find((cp) => cp.id === CHECKPOINT_IDS.START);
  let finishCp = checkpoints.find((cp) => cp.id === CHECKPOINT_IDS.FINISH);

  const totalLength = calculateTrackLength(track);

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
    const startCutoff = checkpoints.find((cp) => cp.id === CHECKPOINT_IDS.START)?.cutoff;
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

// Main setup initialization
export function init(store) {
  const state = store.get();

  // Render setup mode
  renderSetupMode(store);

  // Subscribe to state changes
  store.subscribe((newState) => {
    if (newState.mode !== "setup") return;

    // Checkpoints changed - update checkpoints list
    if (newState.route.checkpoints !== state.route.checkpoints) {
      const checkpointsList = document.getElementById("checkpointsList");
      if (checkpointsList) {
        checkpointsList.innerHTML = renderCheckpointsList(newState);
      }
    }

    // Update map markers
    if (mapInstance) {
      mapInstance.showCheckpoints(newState.route.checkpoints, {
        draggable: true,
        draggableStartFinish: newState.route.track.length === 0,
        onDragEnd: (checkpoint, newCoord) => {
          if (newState.route.track.length > 0) {
            const snapped = snapToTrack(newState.route.track, newCoord);
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
        onClick: (checkpoint) => {
          store.updateNestedSilent("ui.selectedCheckpointId", checkpoint.id);
        },
      });
    }
  });
}

export function renderSetupMode(store) {
  const state = store.get();
  const app = document.querySelector("#app");

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
                ? `<p class="success">✓ ${calculateTrackLength(state.route.track).toFixed(0)} km track (${state.route.segments.length} segment${state.route.segments.length !== 1 ? "s" : ""})</p>`
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

            <button id="saveRoute" class="primary full-width-btn" ${canSaveRoute(state) ? "" : "disabled"}>
              Save & Generate URL
            </button>

            ${!canSaveRoute(state) ? '<p class="hint error-hint">Need: route name, start/finish checkpoints, and cutoff times</p>' : ""}
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

  document.getElementById("gpxFiles").addEventListener("change", (e) => handleGPXUpload(e, store));

  const saveBtn = document.getElementById("saveRoute");
  if (saveBtn) {
    saveBtn.addEventListener("click", () => saveRoute(store));
  }

  // Setup drag and drop for segments
  setupSegmentDragAndDrop();

  // Initialize map
  initSetupMap(store);
}

export function initSetupMap(store) {
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
    const sorted = sortCheckpointsByDistance(state.route.checkpoints);
    const hasStart = sorted.some((cp) => cp.id === CHECKPOINT_IDS.START);
    const hasFinish = sorted.some((cp) => cp.id === CHECKPOINT_IDS.FINISH);

    if (state.route.track.length > 0) {
      const snapped = findCheckpointOnTrack(state.route.track, coord);

      // Show tooltip/popup to confirm
      const popup = L.popup()
        .setLatLng([snapped.coord[1], snapped.coord[0]])
        .setContent(`
          <div class="popup-content">
            <p><strong>Add checkpoint here?</strong></p>
            <p class="popup-subtext">Distance: ${snapped.km.toFixed(1)} km</p>
            <button id="confirmAddCheckpoint" class="popup-btn">Add Checkpoint</button>
          </div>
        `)
        .openOn(mapInstance.getMap());

      // Setup button event listener
      setupPopupButtonListener("confirmAddCheckpoint", () => {
        addCheckpointAt(snapped.coord, snapped.km);
        mapInstance.getMap().closePopup();
      });
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
        .setContent(`
          <div class="popup-content">
            <p><strong>Add ${label} here?</strong></p>
            <button id="confirmAddCheckpoint" class="popup-btn">${label}</button>
          </div>
        `)
        .openOn(mapInstance.getMap());

      // Setup button event listener
      setupPopupButtonListener("confirmAddCheckpoint", () => {
        addCheckpointAtClick(coord, checkpointType);
        mapInstance.getMap().closePopup();
      });
    }
  });

  // Fit map to content
  mapInstance.fitToContent();
}

export async function handleGPXUpload(e, store) {
  const files = Array.from(e.target.files);
  const state = store.get();
  const newSegments = [];

  for (const file of files) {
    try {
      const text = await file.text();
      const coords = parseGPX(text);

      if (coords.length === 0) {
        showInlineMessage(
          document.querySelector(".setup-section"),
          `No valid track data found in ${file.name}`,
          "error"
        );
        continue;
      }

      const simplified = simplifyTrack(coords);
      newSegments.push({
        id: Math.random().toString(36).substring(2, 10),
        name: file.name,
        coords,
        length: calculateTrackLength(coords),
      });
    } catch (error) {
      console.error("Error parsing GPX file:", error);
      showInlineMessage(
        document.querySelector(".setup-section"),
        `Error parsing ${file.name}: ${error.message}`,
        "error"
      );
    }
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

  // Setup drag and drop for segments after rendering
  setupSegmentDragAndDrop();
}

function combineSegments(segments) {
  if (segments.length === 0) return [];
  return segments.flatMap((seg) => seg.coords);
}

export function setupSegmentDragAndDrop() {
  const items = document.querySelectorAll(".segment-item");
  let draggedItem = null;
  let draggedId = null;

  // Add drag and drop event listeners
  items.forEach((item) => {
    item.setAttribute("draggable", "true");

    item.addEventListener("dragstart", (e) => {
      draggedItem = e.target;
      draggedId = e.target.dataset.id;
      e.target.classList.add("dragging");
      e.dataTransfer.effectAllowed = "move";
      e.dataTransfer.setData("text/plain", draggedId);
    });

    item.addEventListener("dragend", (e) => {
      e.target.classList.remove("dragging");
    });

    item.addEventListener("dragover", (e) => {
      e.preventDefault();
      e.dataTransfer.dropEffect = "move";
      const target = e.target.closest(".segment-item");
      if (target && target !== draggedItem) {
        const container = target.parentElement;
        const rect = target.getBoundingClientRect();
        const midY = rect.top + rect.height / 2;

        if (e.clientY < midY) {
          container.insertBefore(draggedItem, target);
        } else {
          container.insertBefore(draggedItem, target.nextSibling);
        }
      }
    });

    item.addEventListener("dragenter", (e) => {
      e.preventDefault();
      if (e.target !== draggedItem) {
        e.target.classList.add("drag-over");
      }
    });

    item.addEventListener("dragleave", (e) => {
      e.target.classList.remove("drag-over");
    });

    item.addEventListener("drop", (e) => {
      e.preventDefault();
      e.target.classList.remove("drag-over");
    });
  });

  // Delete buttons
  document.querySelectorAll(".delete-segment").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      e.stopPropagation();
      deleteSegment(e.target.dataset.id);
    });
  });

  // Update segment order in state when drag ends
  document.addEventListener("dragend", (e) => {
    if (draggedId) {
      const newOrder = Array.from(
        document.querySelectorAll(".segment-item"),
      ).map((item) => item.dataset.id);
      const state = store.get();
      const segments = [...state.route.segments];
      const newSegments = newOrder
        .map((id) => segments.find((s) => s.id === id))
        .filter(Boolean);

      store.update({
        route: {
          ...state.route,
          segments: newSegments,
        },
      });

      draggedId = null;
    }
  });
}

function setupPopupButtonListener(buttonId, callback) {
  // Use MutationObserver to wait for button to be added to DOM
  const observer = new MutationObserver((mutations, obs) => {
    const button = document.getElementById(buttonId);
    if (button) {
      button.addEventListener("click", () => {
        callback();
        obs.disconnect();
      });
      obs.disconnect();
    }
  });

  observer.observe(document.body, {
    childList: true,
    subtree: true,
  });
}

export function addCheckpointAt(coord, km) {
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
  const finishIndex = checkpoints.findIndex((cp) => cp.id === CHECKPOINT_IDS.FINISH);

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
  setupCheckpointListeners();
}

export function addCheckpointAtClick(coord, type) {
  const state = store.get();

  if (type === "start") {
    const newCheckpoint = {
      id: CHECKPOINT_IDS.START,
      name: "Start",
      km: 0,
      coord,
      // Default to current time as placeholder for event start
      cutoff: new Date().toISOString().slice(0, 16) + ":00",
    };

    store.update({
      route: {
        ...state.route,
        checkpoints: [...state.route.checkpoints, newCheckpoint],
      },
    });
  } else if (type === "finish") {
    const totalLength = state.route.track.length > 0
      ? calculateTrackLength(state.route.track)
      : 0;

    const newCheckpoint = {
      id: CHECKPOINT_IDS.FINISH,
      name: "Finish",
      km: totalLength,
      coord,
      // Default to start time + 7 days
      cutoff: new Date(Date.now() + 7 * 24 * 60 * 60 * 1000)
        .toISOString()
        .slice(0, 16) + ":00",
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
}

export function setupCheckpointListeners() {
  // Use event delegation instead of setting up individual listeners
  const checkpointsTable = document.getElementById("checkpointsTableBody");
  if (checkpointsTable) {
    checkpointsTable.addEventListener("change", (e) => {
      const target = e.target;
      const checkpointId = target.dataset.id;
      if (!checkpointId) return;

      if (target.classList.contains("cp-name")) {
        updateCheckpoint(checkpointId, { name: target.value });
      } else if (target.classList.contains("cp-km")) {
        const km = parseFloat(target.value);
        const state = store.get();

        if (state.route.track.length > 0) {
          // With track - calculate coordinate from km
          const coord = getCoordAtKm(state.route.track, km);
          updateCheckpoint(checkpointId, { km, coord: coord || [0, 0] });
        } else {
          // No track - just update km, keep existing coordinate
          updateCheckpoint(checkpointId, { km });
        }
      } else if (target.classList.contains("cp-cutoff-type")) {
        const type = target.value;
        if (type === "absolute") {
          updateCheckpoint(checkpointId, {
            cutoff: new Date().toISOString().slice(0, 16),
          });
        } else if (type === "relative") {
          updateCheckpoint(checkpointId, { cutoffHours: 0 });
        }
      } else if (target.classList.contains("cp-cutoff")) {
        updateCheckpoint(checkpointId, { cutoff: target.value + ":00" });
      } else if (target.classList.contains("cp-hours")) {
        updateCheckpoint(checkpointId, {
          cutoffHours: parseFloat(target.value),
        });
      }
    });

    // Setup event delegation for delete buttons
    checkpointsTable.addEventListener("click", (e) => {
      if (e.target.classList.contains("delete-cp")) {
        deleteCheckpoint(e.target.dataset.id);
      }
    });
  }
}

export function setupCheckpointDragAndDrop() {
  // Checkpoint drag and drop functionality will be implemented in a future update
  // For now, checkpoints can be reordered by editing km values
}

export function updateCheckpoint(id, updates) {
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

export function deleteCheckpoint(id) {
  const state = store.get();
  const checkpoints = state.route.checkpoints.filter((cp) => cp.id !== id);

  store.update({
    route: {
      ...state.route,
      checkpoints,
    },
  });
}

export function deleteSegment(id) {
  const state = store.get();
  const segments = state.route.segments.filter((seg) => seg.id !== id);
  const combinedTrack = combineSegments(segments);

  // Update or create start/finish checkpoints
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

export async function saveRoute(store) {
  const state = store.get();

  const missingCutoffs = findMissingCutoffs(state.route.checkpoints);
  if (missingCutoffs.length > 0) {
    const saveBtn = document.getElementById("saveRoute");
    const saveSection = saveBtn?.parentElement;

    if (saveSection) {
      const existingHints = saveSection.querySelectorAll('.hint.error-hint');
      existingHints.forEach(hint => hint.remove());

      showInlineMessage(
        saveSection,
        "Need: route name, start/finish checkpoints, and cutoff times",
        "error",
        false
      );
    }
    return;
  }

  const route = {
    name: state.route.name,
    created: new Date().toISOString(),
    track: state.route.track,
    checkpoints: state.route.checkpoints,
  };

  const routeUrl = saveRouteToURL(route);

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

  // Save tracking data
  const tracking = {
    routeId: route.created,
    arrivals: {},
  };
  saveTracking(tracking);

  // Show success message
  const saveBtn = document.getElementById("saveRoute");
  const saveSection = saveBtn?.parentElement;
  if (saveSection) {
    showInlineMessage(
      saveSection,
      "Route saved! Switching to tracking mode...",
      "success"
    );
  }
}

// Helper functions for rendering
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
            const isStartOrFinish = cp.id === CHECKPOINT_IDS.START || cp.id === CHECKPOINT_IDS.FINISH;
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
                ${isStartOrFinish && cp.id === CHECKPOINT_IDS.START ? '<div class="event-start-hint">Event start time</div>' : ""}
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
