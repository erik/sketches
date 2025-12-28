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
  // Ensure the loaded route has all required properties
  const defaultRoute = createInitialState().route;
  const mergedRoute = {
    ...defaultRoute,
    ...route,
    // Ensure track is always an array
    track: route.track || defaultRoute.track,
    // Ensure segments is always an array
    segments: route.segments || defaultRoute.segments,
    // Ensure checkpoints is always an array
    checkpoints: route.checkpoints || defaultRoute.checkpoints,
  };

  store.update({
    route: mergedRoute,
    mode: "setup",
  });
}

if (tracking) {
  store.update({
    tracking,
    mode: "tracking",
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
