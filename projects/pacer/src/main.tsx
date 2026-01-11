import "./style.css";
import { loadRouteFromURL } from "./shared/storage.js";

import { Livewire, createElement as h, htmlTemplate } from "./livewire.js";
import { createApp as createSetupApp } from "./setup/app.jsx";

export type GlobalStoreProps = {
  mode: "PACE_TRACKER" | "SETUP";
  units: "METRIC" | "IMPERIAL";
};

async function init() {
  const storedRoute = await loadRouteFromURL();
  const initialMode = "PACE_TRACKER"; // storedRoute ? "PACE_TRACKER" : "CONFIGURE";

  const store = new Livewire<GlobalStoreProps>({
    mode: initialMode,
    units: "METRIC",
  });

  const toggleTheme = () => {
    const theme =
      document.documentElement.getAttribute("data-theme") === "dark"
        ? "light"
        : "dark";
    document.documentElement.setAttribute("data-theme", theme);
  };

  document.querySelector("#app").appendChild(
    <div>
      <div class="navbar bg-base-200 shadow-sm border-b-2 border-base-200">
        <div class="flex-1">
          <span class="btn btn-ghost text-lg">sorelegs</span>
        </div>
        <div class="flex-none">
          <button onClick={toggleTheme} class="btn btn-square btn-ghost">
            {htmlTemplate`
              <svg xmlns="http://www.w3.org/2000/svg"
                  fill="none" viewBox="0 0 24 24"
                  class="inline-block h-5 w-5 stroke-current">
                    <path stroke-linecap="round"
                          stroke-linejoin="round"
                          stroke-width="2" d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z">
                    </path>
                </svg>`}
          </button>
        </div>
      </div>

      <store.reactive keys={["mode"]}>
        {({ mode }) =>
          mode === "CONFIGURE"
            ? createSetupApp(store)
            : createPaceTrackerApp(store)
        }
      </store.reactive>
    </div>,
  );
}

function createPaceTrackerApp(globalStore) {
  return h.h1({}, "todo, tracking ui");
}

init();
