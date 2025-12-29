import "./style.css";
import { loadRouteFromURL } from "./shared/storage.js";

import { Livewire, createElement as h } from "./livewire.js";
import { createApp as createSetupApp } from "./setup/app.jsx";

async function init() {
  const storedRoute = await loadRouteFromURL();
  const initialMode = storedRoute ? "PACE_TRACKER" : "CONFIGURE";

  const store = new Livewire({ mode: initialMode });
  store.watch((s) => console.log("state change", JSON.stringify(s)));

  document
    .querySelector("#app")
    .appendChild(
      store.render(["mode"], ({ mode }) =>
        mode === "CONFIGURE"
          ? createSetupApp(store)
          : createPaceTrackerApp(store),
      ),
    );
}

function createPaceTrackerApp(globalStore) {
  return h.h1({}, "todo, tracking ui");
}

init();
