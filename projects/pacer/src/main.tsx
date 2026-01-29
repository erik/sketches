import "./style.css";
import "temporal-polyfill/global";

import {
  loadRouteFromURL,
  setUrlToEvent,
  loadFromLocalStorage,
} from "./shared/storage.js";
import { EventConfig } from "./shared/index.js";

import { Livewire, htmlTemplate } from "./livewire.js";
import { createApp as createSetupApp } from "./setup/app.jsx";
import { createApp as createPaceTrackerApp } from "./pacer.jsx";
import { createEntryScreen } from "./entry.jsx";

const DARK_THEME = "halloween";
const LIGHT_THEME = "light";

export type GlobalStoreProps = {
  mode: "ENTRY" | "SETUP" | "PACE_TRACKER";
  units: "METRIC" | "IMPERIAL";
  darkmode: boolean;
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

function checkIfEventStarted(event: EventConfig): boolean {
  const storageKey = `tracker-state-${event.id}`;
  const saved = loadFromLocalStorage<{ state: string }>(storageKey);
  return saved?.state === "inprogress";
}

async function init() {
  const storedRoute = await loadRouteFromURL();

  let initialMode: "ENTRY" | "SETUP" | "PACE_TRACKER" = "ENTRY";
  if (storedRoute && checkIfEventStarted(storedRoute)) {
    initialMode = "PACE_TRACKER";
  }

  const store = new Livewire<GlobalStoreProps>({
    mode: initialMode,
    units: "METRIC",
    darkmode: true,
  });

  let currentEvent: EventConfig | null = storedRoute;

  const onSetupComplete = async (eventConfig: EventConfig) => {
    currentEvent = eventConfig;
    await setUrlToEvent(eventConfig);
    store.$.mode = "PACE_TRACKER";
  };

  const onEntryAction = (action: "new" | "continue" | null) => {
    console.log("onEntryAction called with:", action);
    if (action === "new") {
      store.$.mode = "SETUP";
    } else if (action === "continue") {
      store.$.mode = "PACE_TRACKER";
    }
  };

  store.watch(["darkmode"], ({ darkmode }) => {
    document.documentElement.setAttribute(
      "data-theme",
      darkmode ? DARK_THEME : LIGHT_THEME,
    );
  });

  const toggleTheme = () => {
    store.$.darkmode = !store.$.darkmode;
  };

  document.querySelector("#app").appendChild(
    <div class="mx-auto max-w-5xl">
      <div class="navbar bg-base-200 shadow-sm border-b-2 border-base-200">
        <div class="flex-1">
          <span class="btn btn-ghost text-lg">sorelegs</span>
        </div>
        <div class="flex-none gap-2">
          <button
            onClick={() => {
              localStorage.clear();
              window.location.reload();
            }}
            class="btn btn-sm btn-ghost"
          >
            Clear Storage
          </button>
          <button onClick={toggleTheme} class="btn btn-square btn-ghost">
            {THEME_ICON}
          </button>
        </div>
      </div>

      {store.render("mode", ({ mode }) => {
        if (mode === "ENTRY") {
          return createEntryScreen(currentEvent, onEntryAction);
        } else if (mode === "SETUP") {
          return createSetupApp(store, onSetupComplete);
        } else {
          return createPaceTrackerApp(store, currentEvent);
        }
      })}
    </div>,
  );
}

init();
