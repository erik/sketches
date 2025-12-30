import "./style.css";
import { loadRouteFromURL } from "./shared/storage.js";

import { Livewire, createElement as h, htmlTemplate } from "./livewire.js";
import { createApp as createSetupApp } from "./setup/app.jsx";

async function init() {
  const storedRoute = await loadRouteFromURL();
  const initialMode = storedRoute ? "PACE_TRACKER" : "CONFIGURE";

  const store = new Livewire({ mode: initialMode });
  store.watch((s) => console.log("state change", JSON.stringify(s)));

  document.querySelector("#app").appendChild(
    h.div({}, [
      htmlTemplate`
          <div class="navbar bg-base-200 shadow-sm border-b-2 border-base-200">
            <div class="flex-1">
              <span class="btn btn-ghost text-lg">sorelegs</span>
            </div>
            <div class="flex-none dropdown dropdown-end">
              <button tabindex="0" class="btn btn-square btn-ghost">
                <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" class="inline-block h-5 w-5 stroke-current">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 12h.01M12 12h.01M19 12h.01M6 12a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0z"></path>
                </svg>
              </button>
              <ul
                tabindex="-1"
                class="menu menu-sm dropdown-content bg-base-100 rounded-box z-999 mt-3 w-52 p-2 shadow">
                <li>Units</li>
                <li>Light</li>
              </ul>
            </div>
          </div>
      `,
      store.render(["mode"], ({ mode }) => {
        return mode === "CONFIGURE"
          ? createSetupApp(store)
          : createPaceTrackerApp(store);
      }),
    ]),
  );
}

function createPaceTrackerApp(globalStore) {
  return h.h1({}, "todo, tracking ui");
}

init();
