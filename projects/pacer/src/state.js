/**
 * Simple reactive store for application state.
 * No globals - state is passed explicitly.
 */

/**
 * @typedef {Object} Checkpoint
 * @property {string} id - Unique identifier
 * @property {string} name - Display name
 * @property {number} km - Distance from start in km
 * @property {[number, number]} coord - [lng, lat]
 * @property {string} [cutoff] - Absolute ISO timestamp
 * @property {number} [cutoffHours] - Hours from start (alternative to cutoff)
 */

/**
 * @typedef {Object} TrackSegment
 * @property {string} id - Unique identifier
 * @property {string} name - Original filename
 * @property {[number, number][]} coords - Simplified track as [lng, lat] pairs
 * @property {number} length - Length in km
 */

/**
 * @typedef {Object} Route
 * @property {string} name - Route name
 * @property {string} created - ISO timestamp of creation
 * @property {TrackSegment[]} segments - Ordered list of track segments
 * @property {[number, number][]} track - Combined simplified track as [lng, lat] pairs
 * @property {Checkpoint[]} checkpoints - Ordered list of checkpoints
 */

/**
 * @typedef {Object} Tracking
 * @property {string} routeId - Matches route.created to verify correct route
 * @property {Object<string, string>} arrivals - Map of checkpoint ID to arrival ISO timestamp
 */

/**
 * @typedef {Object} UIState
 * @property {string|null} selectedCheckpointId
 * @property {string|null} editingCheckpointId
 * @property {[number, number]|null} userLocation - Raw GPS [lng, lat]
 * @property {{coord: [number, number], km: number}|null} snappedLocation - Snapped to track
 */

/**
 * @typedef {Object} AppState
 * @property {'setup'|'tracking'} mode
 * @property {Route} route
 * @property {Tracking} tracking
 * @property {UIState} ui
 */

/**
 * Creates a reactive store.
 * @param {T} initial - Initial state
 * @returns {{get: () => T, update: (partial: Partial<T>) => void, subscribe: (fn: (state: T) => void) => () => void}}
 * @template T
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
 * @returns {AppState}
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
    },
  };
}

/**
 * Generate a unique ID for checkpoints.
 * @returns {string}
 */
export function generateId() {
  return "cp_" + Math.random().toString(36).substring(2, 10);
}

/**
 * Get the cutoff time for a checkpoint.
 * @param {Checkpoint} checkpoint
 * @param {Date|null} startTime - Start time (required if using cutoffHours)
 * @returns {Date|null}
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
 * @param {Checkpoint[]} checkpoints
 * @param {string} id
 * @returns {Checkpoint|undefined}
 */
export function findCheckpoint(checkpoints, id) {
  return checkpoints.find((cp) => cp.id === id);
}

/**
 * Get arrival time for a checkpoint.
 * @param {Tracking} tracking
 * @param {string} checkpointId
 * @returns {Date|null}
 */
export function getArrivalTime(tracking, checkpointId) {
  const iso = tracking.arrivals[checkpointId];
  return iso ? new Date(iso) : null;
}

/**
 * Get the start checkpoint (first one, assumed to be km=0).
 * @param {Route} route
 * @returns {Checkpoint|undefined}
 */
export function getStartCheckpoint(route) {
  return route.checkpoints.length > 0 ? route.checkpoints[0] : undefined;
}

/**
 * Get the start time from tracking data.
 * @param {Route} route
 * @param {Tracking} tracking
 * @returns {Date|null}
 */
export function getStartTime(route, tracking) {
  const startCp = getStartCheckpoint(route);
  if (!startCp) return null;
  return getArrivalTime(tracking, startCp.id);
}
