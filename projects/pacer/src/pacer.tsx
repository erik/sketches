import { type Position } from "geojson";
import L, { LatLngTuple } from "leaflet";

import { Livewire } from "./livewire.js";
import { GlobalStoreProps } from "./main.jsx";
import { type EventConfig } from "./shared/index.js";
import { calculateRoutePosition } from "./shared/geo.js";

import { DEMO_DATA } from "./data.js";
import { createMap } from "./shared/map.js";

const TIME_FORMAT = new Intl.DateTimeFormat(undefined, {
  hour: "2-digit",
  minute: "2-digit",
  hour12: false,
});

const DATE_FORMAT = new Intl.DateTimeFormat(undefined, {
  month: "short",
  day: "numeric",
});

// App state persisted to /restord from LocalStorage between page loads
type EventState = {
  eventStatus: "before" | "during" | "after";
  nextMarkerId: string;
  currentDistance: number;
  lastLocation: Position;
  markerArrivalTimes: Record<string, Temporal.Instant>;
};

export function formatDateTimeCompact(
  instant: Temporal.Instant | null,
): string {
  if (instant == null) return "--";

  const zdt = instant.toZonedDateTimeISO(Temporal.Now.timeZoneId());

  const time = TIME_FORMAT.format(zdt.toInstant());
  const date = DATE_FORMAT.format(zdt.toInstant());
  return `${time} ${date}`;
}

function formatDuration(duration: Temporal.Duration): string {
  const days = Math.floor(duration.total({ unit: "days" }));
  const hours = Math.floor(duration.total({ unit: "hours" }) % 24);
  const minutes = Math.floor(duration.total({ unit: "minutes" }) % 60);

  const parts = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0) parts.push(`${hours}h`);
  if (minutes > 0) parts.push(`${minutes}m`);
  if (parts.length === 0) parts.push("n/a");

  return parts.join(" ");
}

function formatRelativeDistance(
  currentDistance: number,
  totalDistance: number,
): string {
  return `${(totalDistance - currentDistance).toFixed(1)} km remaining`;
}

function formatRelativeTime(time: Temporal.Instant | null): string {
  if (time == null) return "??";

  const now = Temporal.Now.instant();
  const duration = time.since(now);
  // if (duration.total({ unit: "seconds" }) <= 0) return "Arrived";
  return formatDuration(duration.abs());
}

type MarkerVisitStatus = {
  state: "unvisited" | "visited" | "skipped";
  arrivalTime?: Temporal.Instant;
  segmentPace?: number;
};

type StoreProps = {
  state: "unstarted" | "inprogress" | "finished";
  event: EventConfig;
  progress: Record<string, MarkerVisitStatus>;
  userLocation?: Position;
};

type ComputedProps = {
  $currentDistance: number;
  $currentPace: number;
};

type AppState = Livewire<StoreProps, ComputedProps>;

(globalThis as any).faker = null;
function mockUserLocation(store: AppState) {
  if ((globalThis as any).faker) return;

  const fakePoints = [...DEMO_DATA.segments[0].geometry.coordinates].slice(
    15000,
  );
  (globalThis as any).faker = setInterval(() => {
    if (fakePoints.length === 0) {
      clearInterval((globalThis as any).faker);
    } else {
      store.$.userLocation = fakePoints.shift();
    }
  }, 250);
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

const createStore = (g: Livewire<GlobalStoreProps>) => {
  const store = new Livewire<StoreProps, ComputedProps>({
    state: "inprogress",
    event: DEMO_DATA,
    userLocation: undefined,
    progress: {},
  });

  store.compute("$currentDistance", ({ userLocation, event }) => {
    // If we have user location and route data, calculate actual position
    if (userLocation && event.segments.length > 0) {
      const routeCoordinates = event.segments[0].geometry.coordinates as [
        number,
        number,
      ][];
      const { distanceFromStart, distanceFromTrack } = calculateRoutePosition(
        routeCoordinates,
        userLocation as [number, number],
      );

      // If user is very far from route (50+ km), snap to position 0
      if (distanceFromTrack > 50) {
        return 0;
      }

      return distanceFromStart;
    }

    return 0;
  });

  store.compute("$currentPace", ({ event, $currentDistance }) => {
    const now = Temporal.Now.instant();
    const elapsedTime = now.since(event.startTime);
    const hoursElapsed = elapsedTime.total({ unit: "hours" });

    if ($currentDistance > 0 && hoursElapsed > 0) {
      return $currentDistance / hoursElapsed;
    }

    return 0;
  });

  // Automatic checkpoint detection based on current distance
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
            <MapTab store={store} />
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
                    tabStore.reduce(() => ({ activeTab: tab.id }));
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

export function createApp(globalStore: Livewire<GlobalStoreProps>) {
  const store = createStore(globalStore);

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
  <div class="card bg-base-200 p-4 shadow-sm overflow-hidden">
    <div class="stat-title text-sm">{title}</div>
    <div class="overflow-hidden text-ellipsis whitespace-nowrap text-2xl font-bold">
      {value}
    </div>
    {subtitle && (
      <div class="overflow-hidden text-ellipsis whitespace-nowrap text-xs text-gray-500 mt-1">
        {subtitle}
      </div>
    )}
  </div>
);

const StatsTab = ({ store }: { store: AppState }) => {
  return (
    <div class="flex-1 overflow-auto p-4">
      <div class="space-y-6">
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
                nextMarker.routeDistance || event.routeLength || 0,
                nextMarker.cutoffTime,
              );

            const finishEta = calculateEta(
              $currentDistance,
              event.routeLength,
              $currentPace,
            );

            const pacingDelta = finishEta && event.endTime.since(finishEta);

            return (
              <div class="grid grid-cols-2 gap-4">
                <StatCard
                  title="Distance"
                  value={`${$currentDistance.toFixed(1)} km`}
                  subtitle={formatRelativeDistance(
                    $currentDistance,
                    event.routeLength,
                  )}
                />
                <StatCard
                  title="Overall Pace"
                  value={`${$currentPace.toFixed(1)} km/h`}
                />
                {nextMarker && (
                  <>
                    <StatCard
                      title={`Min Pace (${nextMarker.name})`}
                      value={`${minPaceNextCutoff.toFixed(1)} km/h`}
                      subtitle={`Cutoff: ${formatDateTimeCompact(nextMarker.cutoffTime)}`}
                    />
                    <StatCard
                      title={`ETA (${nextMarker.name})`}
                      value={formatDateTimeCompact(nextMarker.cutoffTime)}
                      subtitle={formatRelativeTime(
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
                  value={formatDateTimeCompact(finishEta)}
                  subtitle={formatRelativeTime(finishEta)}
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
  currentDist: number,
  totalDist: number,
  targetTime: Temporal.Instant | null,
): number | null {
  if (!targetTime) return null;

  const now = Temporal.Now.instant();
  const duration = targetTime.since(now);

  if (currentDist >= totalDist || duration.total({ unit: "seconds" }) <= 0)
    return null;

  const timeRemain = duration.total({ unit: "hours" });
  const distRemain = totalDist - currentDist;

  return distRemain / timeRemain;
}

function calculateEta(
  currentDist: number,
  totalDist: number,
  pace: number,
): Temporal.Instant | null {
  if (pace <= 0) return null;

  const distRemain = totalDist - currentDist;
  if (distRemain <= 0) return null;

  const now = Temporal.Now.instant();
  return now.add({ seconds: Math.round((distRemain / pace) * 3600) });
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

  // Both times exist - prioritize the next upcoming one
  const cutoffPassed = Temporal.Instant.compare(now, cutoff) > 0;
  const goalPassed = Temporal.Instant.compare(now, goal) > 0;

  if (!cutoffPassed && !goalPassed) {
    // Neither passed - show whichever is sooner
    return Temporal.Instant.compare(cutoff, goal) < 0
      ? { time: cutoff, type: "cutoff" }
      : { time: goal, type: "goal" };
  }

  if (cutoffPassed && goalPassed) {
    // Both passed - show the later one
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
              value={`${marker.routeDistance?.toFixed(1) || "?"} km`}
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
          // If not arrived: show distance remaining, ETA, goal/cutoff times
          <>
            <MiniStat
              title="Distance"
              value={`${(marker.routeDistance - $currentDistance).toFixed(1)} km`}
            />
            <MiniStat
              title="ETA"
              value={eta ? formatDateTimeCompact(eta) : "-"}
            />

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

const MapTab = ({ store }: { store: AppState }) => {
  return (
    <div class="h-full w-full relative pb-16">
      <div
        id="map-container"
        class="h-full w-full min-h-75"
        $mount={(el: HTMLElement) => initializeMap(el, store)}
      />
    </div>
  );
};

function initializeMap(
  container: HTMLElement,
  store: Livewire<StoreProps, ComputedProps>,
) {
  const { markers, segments } = store.$.event;
  const map = createMap(container);

  const allCoordinates = segments.map(
    (s) => s.geometry.coordinates,
  ) as LatLngTuple[][];

  map.setRouteMarkers(markers);
  map.setTrack(allCoordinates, { fitBounds: true });

  store.watch(["userLocation"], ({ userLocation }) => {
    const [lng, lat] = userLocation;
    map.setUserLocation({ lat, lng });
  });

  watchUserLocation(store);
}
