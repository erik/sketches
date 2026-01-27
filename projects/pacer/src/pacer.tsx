import { type Position } from "geojson";

import { Livewire } from "./livewire.js";
import { GlobalStoreProps } from "./main.jsx";
import { type EventConfig, Meters, metersToKm } from "./shared/index.js";
import { calculateRoutePosition } from "./shared/geo.js";
import {
  formatDateTimeCompact,
  formatDuration,
  formatRelativeTime,
} from "./shared/time.js";
import { loadFromLocalStorage, saveToLocalStorage } from "./shared/storage.js";

import { DEMO_DATA } from "./data.js";
import { createMap } from "./shared/map.js";

function formatRelativeDistance(
  currentDistance: Meters,
  totalDistance: Meters,
): string {
  return `${metersToKm(Meters(totalDistance - currentDistance)).toFixed(1)} km remaining`;
}

type MarkerVisitStatus = {
  state: "unvisited" | "visited" | "skipped";
  arrivalTime?: Temporal.Instant;
  segmentPace?: number;
};

type TrackerState = {
  state: "unstarted" | "inprogress" | "finished";
  progress: Record<string, MarkerVisitStatus>;
};

function serializeForStorage(state: TrackerState): any {
  return {
    state: state.state,
    progress: Object.fromEntries(
      Object.entries(state.progress).map(([id, marker]) => [
        id,
        {
          ...marker,
          arrivalTime: marker.arrivalTime?.toString(),
        },
      ]),
    ),
  };
}

function deserializeFromStorage(data: any): TrackerState {
  return {
    state: data.state,
    progress: Object.fromEntries(
      Object.entries(data.progress).map(([id, marker]: [string, any]) => [
        id,
        {
          ...marker,
          arrivalTime: marker.arrivalTime
            ? Temporal.Instant.from(marker.arrivalTime)
            : undefined,
        },
      ]),
    ),
  };
}

type StoreProps = {
  state: "unstarted" | "inprogress" | "finished";
  event: EventConfig;
  progress: Record<string, MarkerVisitStatus>;
  userLocation?: Position;
};

type ComputedProps = {
  $currentDistance: Meters;
  $currentPace: number;
};

type AppState = Livewire<StoreProps, ComputedProps>;

(globalThis as any).faker = null;
function mockUserLocation(store: AppState) {
  if ((globalThis as any).faker) return;

  const event = store.$.event;
  if (!event.segments[0]?.geometry?.coordinates) return;

  const fakePoints = [...event.segments[0].geometry.coordinates];
  const interval = Math.max(250, 30000 / fakePoints.length);
  (globalThis as any).faker = setInterval(() => {
    if (fakePoints.length === 0) {
      clearInterval((globalThis as any).faker);
    } else {
      store.$.userLocation = fakePoints.shift();
    }
  }, interval);
}

function watchUserLocation(store: Livewire<StoreProps, ComputedProps>) {
  // navigator.geolocation.getCurrentPosition(
  //   (position) => {
  //     store.$.userLocation = [
  //       position.coords.longitude,
  //       position.coords.latitude,
  //     ];
  //   },
  //   (error) => {
  //     console.error("failed to get location", error);
  //   },
  // );
}

const createStore = (g: Livewire<GlobalStoreProps>, event: EventConfig) => {
  const eventId = event.id;
  const storageKey = `tracker-state-${eventId}`;
  const saved = loadFromLocalStorage(storageKey);
  const savedState = saved ? deserializeFromStorage(saved) : null;

  const store = new Livewire<StoreProps, ComputedProps>({
    state: savedState?.state || "inprogress",
    event: event,
    userLocation: undefined,
    progress: savedState?.progress || {},
  });

  store.compute("$currentDistance", ({ userLocation, event }) => {
    if (userLocation && event.segments.length > 0) {
      const routeCoordinates = event.segments[0].geometry.coordinates as [
        number,
        number,
      ][];
      const { distanceFromStart, distanceFromTrack } = calculateRoutePosition(
        routeCoordinates,
        userLocation as [number, number],
      );

      if (distanceFromTrack > 50) {
        return Meters(0);
      }

      return distanceFromStart;
    }

    return Meters(0);
  });

  store.compute("$currentPace", ({ event, $currentDistance }) => {
    if ($currentDistance === 0) {
      return 0;
    }

    const now = Temporal.Now.instant();
    const elapsedTime = now.since(event.startTime);
    const hoursElapsed = elapsedTime.total({ unit: "hours" });

    if (hoursElapsed > 0.01) {
      return metersToKm($currentDistance) / hoursElapsed;
    }

    return 0;
  });

  store.watch(
    ["$currentDistance"],
    ({ $currentDistance, progress, event, $currentPace }) => {
      const markers = event.markers.filter((m) => m.kind !== "start");

      for (const m of markers) {
        if (!m.routeDistance || progress[m.id]?.state === "visited") return;

        if ($currentDistance >= m.routeDistance) {
          progress[m.id] = {
            state: "visited",
            arrivalTime: Temporal.Now.instant(),
            segmentPace: $currentPace,
          };
        }
      }

      store.$.progress = progress;
    },
  );

  mockUserLocation(store);
  return store;
};

function setupStatePersistence(store: AppState, event: EventConfig) {
  const eventId = event.id;
  const storageKey = `tracker-state-${eventId}`;

  store.watch(["state", "progress"], ({ state, progress }) => {
    const trackerState: TrackerState = { state, progress };
    const serialized = serializeForStorage(trackerState);
    saveToLocalStorage(storageKey, serialized);
  });
}

const TabView = ({
  store,
  globalStore,
}: {
  store: AppState;
  globalStore: Livewire<GlobalStoreProps>;
}) => {
  const tabStore = new Livewire({
    activeTab: "stats",
  });

  const tabs = [
    { id: "stats", label: "Stats" },
    { id: "map", label: "Map" },
    { id: "setup", label: "Setup" },
  ];

  return (
    <tabStore.reactive keys="activeTab">
      {({ activeTab }: { activeTab: string }) => (
        <div class="flex-1 pb-16">
          {activeTab === "stats" ? (
            <StatsTab store={store} />
          ) : (
            <MapTab store={store} globalStore={globalStore} />
          )}
        </div>
      )}
      {({ activeTab }: { activeTab: string }) => (
        <div class="fixed bottom-0 left-0 right-0 z-10">
          <div class="flex border-t border-base-300 bg-base-100">
            {tabs.map((tab) => (
              <button
                key={tab.id}
                class={`flex-1 py-3 px-4 text-center transition-all duration-200 ${activeTab === tab.id ? "bg-primary text-primary-content font-medium" : "bg-base-100 text-base-content hover:bg-base-200"}`}
                onClick={() => {
                  if (tab.id === "setup") {
                    globalStore.$.mode = "SETUP";
                  } else {
                    tabStore.$.activeTab = tab.id;
                  }
                }}
              >
                {tab.label}
              </button>
            ))}
          </div>
        </div>
      )}
    </tabStore.reactive>
  );
};

export function createApp(
  globalStore: Livewire<GlobalStoreProps>,
  event: EventConfig | null,
) {
  const actualEvent = event || DEMO_DATA;
  const store = createStore(globalStore, actualEvent);

  setupStatePersistence(store, actualEvent);

  return (
    <main class="h-dvh flex flex-col bg-base-100">
      <TabView store={store} globalStore={globalStore}></TabView>
    </main>
  );
}

const StatCard = ({
  title,
  value,
  subtitle,
}: {
  title: string;
  value: string;
  subtitle?: string;
}) => (
  <div class="card bg-base-200 p-3 shadow-sm overflow-hidden">
    <div class="stat-title text-sm">{title}</div>
    <div class="overflow-hidden text-ellipsis whitespace-nowrap text-2xl font-bold">
      {value}
    </div>
    {subtitle && (
      <div class="overflow-hidden text-ellipsis whitespace-nowrap text-xs text-gray-500 mt-0.5">
        {subtitle}
      </div>
    )}
  </div>
);

const StatsTab = ({ store }: { store: AppState }) => {
  return (
    <div class="flex-1 overflow-auto p-3">
      <div class="space-y-4">
        <store.reactive
          keys={["$currentDistance", "$currentPace", "event", "progress"]}
        >
          {({
            $currentDistance,
            $currentPace,
            event,
            progress,
          }: StoreProps & ComputedProps) => {
            const markers = event.markers.filter(
              (m) => m.kind !== "start" && m.kind !== "finish",
            );
            const nextMarker = markers.find(
              (m) => m.cutoffTime && progress[m.id]?.state !== "visited",
            );

            const minPaceNextCutoff =
              nextMarker &&
              calculateRequiredPace(
                $currentDistance,
                nextMarker.routeDistance || event.routeLength || Meters(0),
                nextMarker.cutoffTime,
              );

            const finishEta = calculateEta(
              $currentDistance,
              event.routeLength,
              $currentPace,
            );

            const pacingDelta = finishEta && event.endTime.since(finishEta);

            return (
              <div class="grid grid-cols-2 gap-3">
                <StatCard
                  title="Distance"
                  value={`${metersToKm($currentDistance).toFixed(1)} km`}
                  subtitle={formatRelativeDistance(
                    $currentDistance,
                    event.routeLength,
                  )}
                />
                <StatCard
                  title="Overall Pace"
                  value={`${$currentPace.toFixed(1)} km/h`}
                />
                {nextMarker && minPaceNextCutoff && (
                  <>
                    <StatCard
                      title={`Min Pace (${nextMarker.name})`}
                      value={`${minPaceNextCutoff.toFixed(1)} km/h`}
                      subtitle={`Cutoff: ${formatDateTimeCompact(nextMarker.cutoffTime)}`}
                    />
                    <StatCard
                      title={`ETA (${nextMarker.name})`}
                      value={formatRelativeTime(
                        calculateEta(
                          $currentDistance,
                          nextMarker.routeDistance,
                          $currentPace,
                        ),
                      )}
                      subtitle={formatDateTimeCompact(
                        calculateEta(
                          $currentDistance,
                          nextMarker.routeDistance,
                          $currentPace,
                        ),
                      )}
                    />
                  </>
                )}
                <StatCard
                  title="Finish ETA"
                  value={formatRelativeTime(finishEta)}
                  subtitle={formatDateTimeCompact(finishEta)}
                />
                <StatCard
                  title="Pacing"
                  value={pacingDelta ? formatDuration(pacingDelta.abs()) : "--"}
                  subtitle={pacingDelta?.sign >= 0 ? "ahead" : "behind"}
                />
                <StatCard
                  title="Elapsed Time"
                  value={formatRelativeTime(event.startTime)}
                  subtitle={`Started: ${formatDateTimeCompact(event.startTime)}`}
                />
                <StatCard
                  title="Time Remaining"
                  value={formatRelativeTime(event.endTime)}
                  subtitle={`Finish Cutoff: ${formatDateTimeCompact(event.endTime)}`}
                />
              </div>
            );
          }}
        </store.reactive>

        <store.reactive keys={["progress", "$currentDistance"]}>
          {({ event }: StoreProps) =>
            event.markers.map((m, index) => (
              <RouteMarkerCard marker={m} store={store} {...store.$} />
            ))
          }
        </store.reactive>
      </div>
    </div>
  );
};

function calculateRequiredPace(
  currentDist: Meters,
  totalDist: Meters,
  targetTime: Temporal.Instant | null,
): number | null {
  if (!targetTime) return null;

  const now = Temporal.Now.instant();
  const duration = targetTime.since(now);

  if (currentDist >= totalDist || duration.total({ unit: "seconds" }) <= 0)
    return null;

  const timeRemain = duration.total({ unit: "hours" });
  const distRemainKm = metersToKm(Meters(totalDist - currentDist));

  return distRemainKm / timeRemain;
}

function calculateEta(
  currentDist: Meters,
  totalDist: Meters,
  pace: number,
): Temporal.Instant | null {
  if (pace <= 0) return null;

  const distRemainKm = metersToKm(Meters(totalDist - currentDist));
  if (distRemainKm <= 0) return null;

  const now = Temporal.Now.instant();
  return now.add({ seconds: Math.round((distRemainKm / pace) * 3600) });
}

function getNextRelevantTime(
  marker: any,
  now: Temporal.Instant,
): { time: Temporal.Instant; type: "cutoff" | "goal" } | null {
  const cutoff = marker.cutoffTime;
  const goal = marker.goalTime;

  if (!cutoff && !goal) return null;
  if (!cutoff) return { time: goal, type: "goal" };
  if (!goal) return { time: cutoff, type: "cutoff" };

  const cutoffPassed = Temporal.Instant.compare(now, cutoff) > 0;
  const goalPassed = Temporal.Instant.compare(now, goal) > 0;

  if (!cutoffPassed && !goalPassed) {
    return Temporal.Instant.compare(cutoff, goal) < 0
      ? { time: cutoff, type: "cutoff" }
      : { time: goal, type: "goal" };
  }

  if (cutoffPassed && goalPassed) {
    return Temporal.Instant.compare(cutoff, goal) > 0
      ? { time: cutoff, type: "cutoff" }
      : { time: goal, type: "goal" };
  }

  // One passed, one hasn't - show the one that hasn't passed
  return cutoffPassed
    ? { time: goal, type: "goal" }
    : { time: cutoff, type: "cutoff" };
}

const RouteMarkerCard = ({
  marker,
  store,
  progress,
  $currentPace,
  $currentDistance,
  event,
}: {
  marker: any;
  store: any;
  progress: any;
  $currentPace: any;
  $currentDistance: any;
  event: any;
}) => {
  const progressEvent = progress[marker.id];
  const isCompleted = progressEvent?.state === "visited";

  const eta = calculateEta(
    $currentDistance,
    marker.routeDistance,
    $currentPace,
  );
  const requiredPace = calculateRequiredPace(
    $currentDistance,
    marker.routeDistance,
    marker.cutoffTime,
  );
  const goalPace = calculateRequiredPace(
    $currentDistance,
    marker.routeDistance,
    marker.goalTime,
  );

  const handleClearCheckpoint = () => {
    const newProgress = { ...progress };
    delete newProgress[marker.id];
    store.$.progress = newProgress;
  };

  // For start point, don't allow check-in
  const isStartPoint = marker.kind === "start";

  const now = Temporal.Now.instant();
  const nextTime =
    !isCompleted && !isStartPoint ? getNextRelevantTime(marker, now) : null;

  const MiniStat = ({ title, value }: { title: string; value: string }) => (
    <div class="text-xs">
      <div class="text-gray-500">{title}</div>
      <div>{value}</div>
    </div>
  );

  return (
    <div
      class={`card shadow-sm mb-4 overflow-hidden p-4 bg-base-200 rounded-lg ${isCompleted || isStartPoint ? "bg-gray-600/10" : "bg-base-100 border-base-300"}`}
    >
      <div class="flex justify-between items-center mb-3">
        <div class="flex-1">
          <div class="flex items-center gap-2 mb-1">
            <h4 class="font-bold text-md">{marker.name}</h4>
            {nextTime && (
              <span class="badge badge-xs badge-soft">
                {nextTime.type === "cutoff" ? "Cutoff" : "Goal"}:{" "}
                {formatDateTimeCompact(nextTime.time)}
              </span>
            )}
          </div>
          {marker.note && <p class="text-sm text-gray-600">{marker.note}</p>}
        </div>
        {isCompleted && (
          <button
            class="btn btn-xs btn-ghost btn-error"
            onClick={handleClearCheckpoint}
          >
            Reset
          </button>
        )}
      </div>

      <div class="grid grid-cols-3">
        {isStartPoint ? (
          <MiniStat
            title={"Start Time"}
            value={formatDateTimeCompact(event.startTime)}
          />
        ) : isCompleted ? (
          <>
            <MiniStat
              title="Distance"
              value={`${marker.routeDistance ? metersToKm(marker.routeDistance).toFixed(1) : "?"} km`}
            />
            <MiniStat
              title="Pace"
              value={`${progressEvent?.segmentPace?.toFixed(1) || "-"} km/h`}
            />
            <MiniStat
              title="Time"
              value={formatDateTimeCompact(progressEvent.arrivalTime)}
            />
          </>
        ) : (
          <>
            <MiniStat
              title="Distance"
              value={`${metersToKm(Meters(marker.routeDistance - $currentDistance)).toFixed(1)} km`}
            />
            <MiniStat title="ETA" value={eta ? formatRelativeTime(eta) : "-"} />

            {marker.cutoffTime && (
              <MiniStat
                title="Cutoff"
                value={formatDateTimeCompact(marker.cutoffTime)}
              />
            )}

            {marker.goalTime && (
              <MiniStat
                title="Goal"
                value={formatDateTimeCompact(marker.goalTime)}
              />
            )}

            {requiredPace && (
              <MiniStat
                title="Min Pace"
                value={`${requiredPace.toFixed(1)} km/h`}
              />
            )}
            {goalPace && (
              <MiniStat
                title="Goal Pace"
                value={`${goalPace.toFixed(1)} km/h`}
              />
            )}
          </>
        )}
      </div>
    </div>
  );
};

const MapTab = ({
  store,
  globalStore,
}: {
  store: AppState;
  globalStore: Livewire<GlobalStoreProps>;
}) => {
  return (
    <div class="h-full w-full relative pb-16">
      <div
        id="map-container"
        class="h-full w-full min-h-75"
        $mount={(el: HTMLElement) => initializeMap(el, store, globalStore)}
      />
    </div>
  );
};

function initializeMap(
  container: HTMLElement,
  store: Livewire<StoreProps, ComputedProps>,
  globalStore: Livewire<GlobalStoreProps>,
) {
  const { markers, segments } = store.$.event;
  const map = createMap(container, {
    darkmode: globalStore.$.darkmode,
  });

  map.setRouteMarkers(markers);
  map.setTrackSegments(segments, { fitBounds: true });

  store.watch(["userLocation"], ({ userLocation }) => {
    const [lng, lat] = userLocation;
    map.setUserLocation({ lat, lng });
  });

  globalStore.watch(["darkmode"], ({ darkmode }) => {
    map.setDarkMode(darkmode);
  });

  watchUserLocation(store);
}
