```javascript
/**
 * Main Entry Point
 * Initializes the application and routes to appropriate mode
 */
import "./style.css";
import { createStore, createInitialState } from "./shared/state.js";
import { loadRouteFromURL, loadTracking } from "./shared/storage.js";
import { init as initSetup } from "./setup/main.js";
import { init as initTracking } from "./tracking/main.js";

// Create the app store
const store = createStore(createInitialState());

// Load from URL if available
const route = loadRouteFromURL();
const tracking = loadTracking();

if (route) {
  store.update({
    route,
    mode: "setup"
  });
}

if (tracking) {
  store.update({
    tracking,
    mode: "tracking"
  });
}

// Initialize based on mode
function init() {
  const state = store.get();

  if (state.mode === "setup") {
    initSetup(store);
  } else {
    initTracking(store);
  }

  // Subscribe to state changes
  store.subscribe((newState) => {
    if (newState.mode === "setup") {
      initSetup(store);
    } else {
      initTracking(store);
    }
  });
}

// Start the app
init();
```

Now let me update the shared modules to have proper exports. Let me start with the state module:```path/to/file.js
```javascript
/**
 * State Management Module
 * Reactive store and state utilities
 */
export function createStore(initial) {
  let state = structuredClone(initial);
  const listeners = new Set();

  return {
    /** Get current state (returns a reference, don't mutate directly) */
    get() {
      return state;
    },

    /** Update state with partial object, triggers listeners */
    update(partial) {
      state = { ...state, ...partial };
      listeners.forEach((fn) => fn(state));
    },

    /** Update state without triggering listeners (for form inputs) */
    updateSilent(partial) {
      state = { ...state, ...partial };
    },

    /** Deep update for nested properties */
    updateNested(path, value) {
      const keys = path.split(".");
      const newState = structuredClone(state);
      let obj = newState;
      for (let i = 0; i < keys.length - 1; i++) {
        obj = obj[keys[i]];
      }
      obj[keys[keys.length - 1]] = value;
      state = newState;
      listeners.forEach((fn) => fn(state));
    },

    /** Deep update for nested properties without triggering listeners */
    updateNestedSilent(path, value) {
      const keys = path.split(".");
      const newState = structuredClone(state);
      let obj = newState;
      for (let i = 0; i < keys.length - 1; i++) {
        obj = obj[keys[i]];
      }
      obj[keys[keys.length - 1]] = value;
      state = newState;
    },

    /** Subscribe to state changes, returns unsubscribe function */
    subscribe(fn) {
      listeners.add(fn);
      return () => listeners.delete(fn);
    },
  };
}

/**
 * Creates the initial application state.
 * @returns {Object} Initial state
 */
export function createInitialState() {
  return {
    mode: "setup",

    route: {
      name: "",
      created: null,
      segments: [],
      track: [],
      checkpoints: [],
    },

    tracking: {
      routeId: null,
      arrivals: {},
    },

    ui: {
      selectedCheckpointId: null,
      editingCheckpointId: null,
      userLocation: null,
      snappedLocation: null,
      unitSystem: "metric", // "metric" or "imperial"
    },
  };
}

/**
 * Generate a unique ID for checkpoints.
 * @returns {string} Unique ID
 */
export function generateId() {
  return "cp_" + Math.random().toString(36).substring(2, 10);
}

/**
 * Get the cutoff time for a checkpoint.
 * @param {Object} checkpoint - Checkpoint object
 * @param {Date|null} startTime - Start time (required if using cutoffHours)
 * @returns {Date|null} Cutoff time
 */
export function getCutoffTime(checkpoint, startTime) {
  if (checkpoint.cutoff) {
    return new Date(checkpoint.cutoff);
  }
  if (checkpoint.cutoffHours != null && startTime) {
    return new Date(startTime.getTime() + checkpoint.cutoffHours * 3600000);
  }
  return null;
}

/**
 * Find a checkpoint by ID.
 * @param {Array} checkpoints - Checkpoints array
 * @param {string} id - Checkpoint ID
 * @returns {Object|undefined} Found checkpoint
 */
export function findCheckpoint(checkpoints, id) {
  return checkpoints.find((cp) => cp.id === id);
}

/**
 * Get arrival time for a checkpoint.
 * @param {Object} tracking - Tracking data
 * @param {string} checkpointId - Checkpoint ID
 * @returns {Date|null} Arrival time
 */
export function getArrivalTime(tracking, checkpointId) {
  const iso = tracking.arrivals[checkpointId];
  return iso ? new Date(iso) : null;
}

/**
 * Get the start checkpoint (first one, assumed to be km=0).
 * @param {Object} route - Route object
 * @returns {Object|undefined} Start checkpoint
 */
export function getStartCheckpoint(route) {
  return route.checkpoints.length > 0 ? route.checkpoints[0] : undefined;
}

/**
 * Get the start time from tracking data.
 * @param {Object} route - Route object
 * @param {Object} tracking - Tracking data
 * @returns {Date|null} Start time
 */
export function getStartTime(route, tracking) {
  const startCp = getStartCheckpoint(route);
  if (!startCp) return null;
  return getArrivalTime(tracking, startCp.id);
}
