/**
 * Tracking Mode Module
 * Contains all functions specific to route tracking functionality
 */

// Import shared dependencies
import {
  createStore,
  getCutoffTime,
  findCheckpoint,
  getStartCheckpoint,
  getStartTime,
} from "../shared/state.js";
import { calculateTrackLength } from "../shared/geo/index.js";
import {
  showInlineMessage,
  formatDateTime,
  formatCutoffTime,
  createModal,
} from "../shared/ui/index.js";
import {
  calculateCheckpointMetrics,
  canCheckIn,
  findNextCheckpoint,
  getCutoffTimeForCheckpoint,
  validateClearCheckpoint,
  validateSequentialCheckIn,
} from "../shared/checkpoint-ops.js";
import { saveTracking } from "../shared/storage.js";
import { createMap } from "../shared/map.js";
import {
  calculateCurrentPace,
  calculateRequiredPace,
  calculateEstimatedArrival,
  calculateTimeAheadBehind,
  calculateSummaryStats,
  getCurrentCheckpointIndex,
  formatTimeDifference,
  getTimeDifferenceHours,
} from "../shared/pace.js";

// Map instance
let mapInstance = null;

// Main tracking initialization
export function init(store) {
  const state = store.get();

  // Render tracking mode
  renderTrackingMode(store);

  // Subscribe to state changes
  store.subscribe((newState) => {
    if (newState.mode !== "tracking") return;

    // Handle state changes for tracking mode
    if (newState.tracking !== state.tracking) {
      // Update tracking display
      const checkpointsSection = document.querySelector(".checkpoints-section");
      if (checkpointsSection) {
        checkpointsSection.innerHTML = renderTrackingCheckpoints(newState);
      }

      // Update summary
      const summarySection = document.querySelector(".summary-section");
      if (summarySection) {
        summarySection.innerHTML = renderSummary(newState);
      }

      // Update next cutoff
      const nextCutoffSection = document.querySelector(".next-cutoff-section");
      if (nextCutoffSection) {
        nextCutoffSection.innerHTML = renderNextCutoff(newState);
      }
    }
  });
}

export function renderTrackingMode(store) {
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

  // Setup event listeners
  setupTrackingListeners(store);
  initTrackingMap(store);
  autoPopulateStartTime(store);
}

export function initTrackingMap(store) {
  const state = store.get();

  // Clean up existing map
  if (mapInstance) {
    mapInstance.destroy();
  }

  // Create new map
  mapInstance = createMap("map");

  // Show track if available
  if (state.route.track && state.route.track.length > 0) {
    mapInstance.showTrack(state.route.track, { color: "#10b981" });
  }

  // Show checkpoints
  mapInstance.showCheckpoints(state.route.checkpoints, {
    draggable: false,
  });

  // Setup location button
  const locationBtn = document.getElementById("getLocation");
  if (locationBtn) {
    locationBtn.addEventListener("click", () => getUserLocation(store));
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

  // Fit map to content
  mapInstance.fitToContent();
}

export function autoPopulateStartTime(store) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const startCp = sorted[0];

  if (!startCp) return;

  // If start checkpoint doesn't have an arrival time, set it from cutoff
  // Start checkpoint always uses absolute cutoff time, never cutoffHours
  if (!state.tracking.arrivals[startCp.id] && startCp.cutoff) {
    const tracking = {
      ...state.tracking,
      arrivals: {
        ...state.tracking.arrivals,
        [startCp.id]: new Date(startCp.cutoff).toISOString(),
      },
    };

    store.update({ tracking });
    saveTracking(tracking);
  }
}

export function getUserLocation(store) {
  if (!navigator.geolocation) {
    showInlineMessage(
      document.querySelector(".tracking-container"),
      "Geolocation is not supported by your browser",
      "error",
    );
    return;
  }

  const state = store.get();

  navigator.geolocation.getCurrentPosition(
    (position) => {
      const userCoord = [position.coords.longitude, position.coords.latitude];

      // Show user location on map
      mapInstance.showUserLocation(userCoord);

      // If we have a track, snap to it
      if (state.route.track && state.route.track.length > 0) {
        const snapped = snapToTrack(state.route.track, userCoord);
        if (snapped) {
          // Calculate distance to track
          const distToTrack = calculateDistance(userCoord, snapped.coord);

          mapInstance.showSnappedLocation(snapped.coord, snapped.km);

          // Draw dashed line if user is close to route (within 50km)
          if (distToTrack < 50) {
            mapInstance.drawLineToTrack(userCoord, snapped.coord);

            // Find next checkpoint
            const nextCp = findNextCheckpointByKm(snapped.km);
            if (nextCp) {
              const distToNext = nextCp.km - snapped.km;
              const mapSection = document.querySelector(".map-section");
              showInlineMessage(
                mapSection,
                `You're ${distToTrack.toFixed(1)}km from the route, ${distToNext.toFixed(1)}km to ${nextCp.name}`,
                "info",
                false,
              );
            }
          } else {
            // User is far from route
            const mapSection = document.querySelector(".map-section");
            showInlineMessage(
              mapSection,
              "You're more than 50km from the route. Likely not yet racing.",
              "info",
              false,
            );
          }
        }
      } else {
        // No track available
        mapInstance.showUserLocation(userCoord);
        const mapSection = document.querySelector(".map-section");
        showInlineMessage(
          mapSection,
          "No route track available to snap to.",
          "info",
          false,
        );
      }
    },
    (error) => {
      let errorMessage = "Unable to retrieve your location: ";
      switch (error.code) {
        case error.PERMISSION_DENIED:
          errorMessage += "Permission denied";
          break;
        case error.POSITION_UNAVAILABLE:
          errorMessage += "Location information unavailable";
          break;
        case error.TIMEOUT:
          errorMessage += "Request timed out";
          break;
        default:
          errorMessage += "Unknown error";
      }
      showInlineMessage(
        document.querySelector(".tracking-container"),
        errorMessage,
        "error",
      );
    },
  );
}

function findNextCheckpointByKm(km) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  return sorted.find((cp) => cp.km > km) || sorted[sorted.length - 1];
}

export function renderNextCutoff(store) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const { startTime } = getStartInfo(state);

  if (!startTime) {
    return '<div class="next-cutoff-card">No start time set</div>';
  }

  const now = new Date();
  const nextCp = sorted.find((cp) => {
    const cutoffTime = getCutoffTime(cp, startTime);
    return cutoffTime && cutoffTime > now;
  });

  if (!nextCp) {
    return '<div class="next-cutoff-card">No upcoming cutoffs</div>';
  }

  const cutoffTime = getCutoffTime(nextCp, startTime);
  const { days, hours, minutes, isPast } = calculateTimeRemaining(cutoffTime);

  let statusMessage = "";
  let statusClass = "";

  if (cutoffTime > now) {
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
      <div class="cutoff-time">${formatDateTime(cutoffTime)}</div>
      <div class="cutoff-status">${statusMessage}</div>
    </div>
  `;
}

export function renderSummary(store) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const { startTime } = getStartInfo(state);

  const stats = calculateSummaryStats(state.tracking, sorted, startTime);

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

export function renderTrackingCheckpoints(store) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const { startTime } = getStartInfo(state);
  const currentIndex = getCurrentCheckpointIndex(sorted, state.tracking);

  return `
    <table class="tracking-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>km</th>
          <th>Arrival</th>
          <th>Pace</th>
          <th>Required</th>
          <th>Ahead/Behind</th>
          <th>ETA</th>
          <th>Action</th>
        </tr>
      </thead>
      <tbody>
        ${sorted
          .map((cp, index) => {
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

            const isStart = index === 0;
            const canCheckInNow = canCheckIn(cp, index, sorted, state.tracking);

            return `
          <tr class="${hasArrived ? "reached" : index === currentIndex ? "upcoming" : ""}">
            <td>${cp.name}</td>
            <td>${cp.km.toFixed(1)}</td>
            <td>
              ${
                hasArrived
                  ? `<span class="arrival-time" data-id="${cp.id}">${formatDateTime(new Date(arrival))}</span> <button class="edit-arrival" data-id="${cp.id}">✏️</button>`
                  : estimatedArrival
                    ? `<b>ETA:</b> ${formatDateTime(estimatedArrival)}`
                    : "-"
              }
            </td>
            <td class="${currentPace ? (currentPace >= 10 ? "metric-positive" : "metric-negative") : "metric-neutral"}">
              ${index === 0 ? "n/a" : currentPace ? currentPace.toFixed(1) + " km/h" : "-"}
            </td>
            <td class="${requiredPace ? (requiredPace <= 20 && requiredPace > 0 ? "metric-positive" : "metric-negative") : "metric-neutral"}">
              ${index === 0 ? "n/a" : requiredPace ? requiredPace.toFixed(1) + " km/h" : "-"}
            </td>
            <td class="${timeAheadBehind !== null ? (timeAheadBehind > 0 ? "metric-positive" : "metric-negative") : "metric-neutral"}">
              ${index === 0 ? "n/a" : timeAheadBehind !== null ? (timeAheadBehind > 0 ? "+" : "") + timeAheadBehind.toFixed(1) + "h" : "-"}
            </td>
            <td>${remainingTimeStr || "-"}</td>
            <td>
              ${
                hasArrived
                  ? `<button class="clear-arrival" data-id="${cp.id}">Clear</button>`
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

export function formatCutoff(cp, state) {
  return formatCutoffTime(cp, state);
}

export function setupTrackingListeners(store) {
  document.querySelectorAll(".check-in").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      checkInAt(e.target.dataset.id, store);
    });
  });

  document.querySelectorAll(".clear-arrival").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      clearArrival(e.target.dataset.id, store);
    });
  });

  document.querySelectorAll(".edit-arrival").forEach((btn) => {
    btn.addEventListener("click", (e) => {
      editArrival(e.target.dataset.id, store);
    });
  });
}

export function checkInAt(checkpointId, store) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);

  // Verify all previous checkpoints are checked in
  const validation = validateSequentialCheckIn(
    checkpointId,
    sorted,
    state.tracking,
  );
  // NOTE: Buttons are already disabled in the UI when check-in is not valid
  // This validation is a safety measure for edge cases
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

  // Show success message
  const checkpointsSection = document.querySelector(".checkpoints-section");
  if (checkpointsSection) {
    showInlineMessage(
      checkpointsSection,
      `Checked in at ${sorted.find((cp) => cp.id === checkpointId)?.name}!`,
      "success",
    );
  }
}

export function clearArrival(checkpointId, store) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);

  // Validate that checkpoint can be cleared (e.g., cannot clear if subsequent checkpoints are already checked in)
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

  // Show success message
  const checkpointsSection = document.querySelector(".checkpoints-section");
  if (checkpointsSection) {
    showInlineMessage(
      checkpointsSection,
      `Cleared arrival at ${sorted.find((cp) => cp.id === checkpointId)?.name}`,
      "success",
    );
  }
}

export function editArrival(checkpointId, store) {
  const state = store.get();
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const checkpoint = sorted.find((cp) => cp.id === checkpointId);

  if (!checkpoint) return;

  const currentTime = state.tracking.arrivals[checkpointId];
  const currentDate = currentTime ? new Date(currentTime) : new Date();
  const formatted = formatDateTimeLocal(currentDate);

  const modalContent = `
    <div class="edit-time-modal-content">
      <h3>Edit Arrival Time for ${checkpoint.name}</h3>
      <input type="datetime-local" id="editTimeInput" value="${formatted}">
      <div class="edit-time-buttons">
        <button id="saveEditTime" class="primary">Save</button>
        <button id="cancelEditTime">Cancel</button>
      </div>
    </div>
  `;

  const modal = createModal(modalContent, () => {
    document.removeEventListener("click", handleOutsideClick);
  });

  document.body.appendChild(modal);

  // Handle save
  document.getElementById("saveEditTime").addEventListener("click", () => {
    const input = document.getElementById("editTimeInput");
    const newTime = input.value;

    const tracking = {
      ...state.tracking,
      arrivals: {
        ...state.tracking.arrivals,
        [checkpointId]: new Date(newTime).toISOString(),
      },
    };

    store.update({ tracking });
    saveTracking(tracking);
    modal.remove();

    // Show success message
    const checkpointsSection = document.querySelector(".checkpoints-section");
    if (checkpointsSection) {
      showInlineMessage(
        checkpointsSection,
        `Updated arrival time for ${checkpoint.name}`,
        "success",
      );
    }
  });

  // Handle cancel
  document.getElementById("cancelEditTime").addEventListener("click", () => {
    modal.remove();
  });
}
