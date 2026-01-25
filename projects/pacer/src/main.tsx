import "./style.css";
import "temporal-polyfill/global";

import { loadRouteFromURL } from "./shared/storage.js";

import { Livewire, htmlTemplate } from "./livewire.js";
import { createApp as createSetupApp } from "./setup/app.jsx";
import { createApp as createPaceTrackerApp } from "./pacer.jsx";

export type GlobalStoreProps = {
  mode: "PACE_TRACKER" | "SETUP";
  units: "METRIC" | "IMPERIAL";
  theme: "light" | "dark";
};

const THEME_ICON = htmlTemplate`
  <svg xmlns="http://www.w3.org/2000/svg"
    fill="none" viewBox="0 0 24 24"
    class="inline-block h-5 w-5 stroke-current">
    <path stroke-linecap="round"
          stroke-linejoin="round"
          stroke-width="2" d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z">
    </path>
</svg>`;

async function init() {
  const storedRoute = await loadRouteFromURL();
  const initialMode = storedRoute ? "PACE_TRACKER" : "SETUP";

  const store = new Livewire<GlobalStoreProps>({
    mode: initialMode,
    units: "METRIC",
    theme: "dark",
  });

  // Set initial theme
  const initialDaisyTheme = store.$.theme === "dark" ? "halloween" : "light";
  document.documentElement.setAttribute("data-theme", initialDaisyTheme);

  const toggleTheme = () => {
    const newTheme = store.$.theme === "dark" ? "light" : "dark";
    store.$.theme = newTheme;
    const daisyTheme = newTheme === "dark" ? "halloween" : "light";
    document.documentElement.setAttribute("data-theme", daisyTheme);
  };

  document.querySelector("#app").appendChild(
    <div class="mx-auto max-w-5xl">
      <div class="navbar bg-base-200 shadow-sm border-b-2 border-base-200">
        <div class="flex-1">
          <span class="btn btn-ghost text-lg">sorelegs</span>
        </div>
        <div class="flex-none">
          <button onClick={toggleTheme} class="btn btn-square btn-ghost">
            {THEME_ICON}
          </button>
        </div>
      </div>

      <store.reactive keys={["mode"]}>
        {({ mode }: { mode: string }) =>
          mode === "SETUP" ? createSetupApp(store) : createPaceTrackerApp(store)
        }
      </store.reactive>
    </div>,
  );
}

init();
