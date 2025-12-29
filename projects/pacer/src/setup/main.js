import {
  parseGPX,
  simplifyTrack,
  calculateTrackLength,
  snapToTrack,
} from "../shared/geo/index.js";
import { showInlineMessage } from "../shared/ui";
import { createMap } from "../shared/map.js";

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
      });
    }
  });
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
  if (state.route.track && state.route.track.length > 0) {
    mapInstance.showTrack(state.route.track);
  }

  // Show checkpoints
  mapInstance.showCheckpoints(state.route.checkpoints, {
    draggable: true,
    draggableStartFinish: !state.route.track || state.route.track.length === 0, // Allow dragging start/finish if no track
    onDragEnd: (checkpoint, newCoord) => {
      // Snap to track if available
      if (state.route.track && state.route.track.length > 0) {
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
    onClick: (checkpoint) => {
      // Select checkpoint for editing
      store.updateNestedSilent("ui.selectedCheckpointId", checkpoint.id);
    },
  });
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
          "error",
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
        "error",
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
