import { type LineString, type Position } from "geojson";
import L, { LatLngTuple } from "leaflet";

import { Livewire } from "./livewire.js";
import { GlobalStoreProps } from "./main.jsx";
import { type EventConfig } from "./shared/index.js";

/// <reference types="temporal-spec" />

import { DEMO_DATA } from "./data.js";
import { createMap } from "./shared/map.js";

type EventState = {
  eventStatus: "before" | "during" | "after";
  nextMarkerId: string;
  currentDistance: number;
  markerArrivalTimes: Record<string, Temporal.Instant>;
};

function formatDateTimeCompact(instant: Temporal.Instant | null): string {
  if (!instant) return "?null?";
  const zdt = instant.toZonedDateTimeISO("UTC");

  const timeFormatter = new Intl.DateTimeFormat(undefined, {
    hour: "2-digit",
    minute: "2-digit",
    hour12: false,
    timeZone: "UTC",
  });

  const dateFormatter = new Intl.DateTimeFormat(undefined, {
    month: "short",
    day: "numeric",
    timeZone: "UTC",
  });

  const time = timeFormatter.format(zdt.toInstant());
  const date = dateFormatter.format(zdt.toInstant());

  return `${time} ${date}`;
}

function formatDuration(duration: Temporal.Duration): string {
  const days = Math.floor(duration.total({ unit: "days" }));
  const hours = Math.floor(duration.total({ unit: "hours" }) % 24);
  const minutes = Math.floor(duration.total({ unit: "minutes" }) % 60);
  const seconds = Math.floor(duration.total({ unit: "seconds" }) % 60);

  const parts = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0) parts.push(`${hours}h`);
  if (minutes > 0) parts.push(`${minutes}m`);
  if (seconds > 0) parts.push(`${seconds}s`);
  if (parts.length === 0) parts.push("0s");

  return parts.join(" ");
}

function getTimeRemaining(endTime: Temporal.Instant): string {
  const now = Temporal.Now.instant();
  const remainingDuration = endTime.since(now);

  if (remainingDuration.total({ unit: "seconds" }) <= 0)
    return "Event completed";

  return formatDuration(remainingDuration);
}

function getRemainingDistance(
  event: EventConfig,
  currentDistance: number,
): string {
  if (!event.routeLength) return "No route data";
  return `${(event.routeLength - currentDistance).toFixed(1)} km remaining`;
}

function calculateElapsedTime(event: EventConfig): string {
  const now = Temporal.Now.instant();
  const startTime = event.startTime || now;
  const elapsedDuration = now.since(startTime);
  return formatDuration(elapsedDuration);
}

function getEtaRemainingTime(eta: Temporal.Instant | null): string {
  if (eta == null) return "??";

  const now = Temporal.Now.instant();
  const remainingDuration = eta.since(now);
  if (remainingDuration.total({ unit: "seconds" }) <= 0) return "Arrived";
  return "in " + formatDuration(remainingDuration);
}

type ProgressEvent = {
  markerId: string;
  arrivalTime: Temporal.Instant;
  segmentPace: number;
};

type StoreProps = {
  state: "unstarted" | "inprogress" | "finished";
  event: EventConfig;
  progress: ProgressEvent[];
  userLocation?: Position;
};

type ComputedProps = {
  $currentDistance: number;
  $currentPace: number;
  $requiredPace: number;
  $eta: Temporal.Instant;
};

function handleUserLocation(store: Livewire<StoreProps, ComputedProps>) {
  navigator.geolocation.getCurrentPosition(
    (position) => {
      store.$.userLocation = [
        position.coords.longitude,
        position.coords.latitude,
      ];
    },
    (error) => {
      console.error("failed to get location", error);
    },
  );
}

const createStore = (g: Livewire<GlobalStoreProps>) => {
  const store = new Livewire<StoreProps, ComputedProps>(
    {
      state: "inprogress",
      event: DEMO_DATA,
      userLocation: undefined,
      progress: [
        // Dummy progress data for demo
        {
          markerId: "m0",
          arrivalTime: Temporal.Instant.from("2026-04-26T06:00:00Z"),
          segmentPace: 18.5,
        },
      ],
    },
    {
      parent: g,
    },
  );

  handleUserLocation(store);

  store.compute("$currentDistance", ({ progress }) => {
    if (progress.length > 0) {
      // TODO: Implement proper distance calculation with track snapping
      return progress[progress.length - 1].segmentPace * 2 || 0;
    }
    return 0;
  });

  store.compute("$currentPace", ({ userLocation, progress }) => {
    // TODO: Implement actual pace calculation
    if (progress.length > 0) {
      return progress[progress.length - 1].segmentPace;
    }
    return 0;
  });

  store.compute("$requiredPace", ({ event, progress }) => {
    // Calculate required pace to reach next control point
    const now = Temporal.Now.instant();
    const markers = event.markers.filter(
      (m) => m.kind === "control" || m.kind === "finish",
    );

    if (!markers.length) return 0;

    // Find next control point that hasn't been reached yet
    const nextMarker = markers.find(
      (m) => !progress.some((p) => p.markerId === m.id),
    );

    if (!nextMarker || !nextMarker.cutoffTime) return 0;

    const timeRemainingDuration = nextMarker.cutoffTime.since(now);
    if (timeRemainingDuration.total({ unit: "seconds" }) <= 0) return 0; // Cutoff already passed

    const hoursRemaining = timeRemainingDuration.total({ unit: "hours" });
    const distanceToFinish = nextMarker.routeDistance || event.routeLength || 0;

    return distanceToFinish / hoursRemaining;
  });

  store.compute("$eta", ({ event, $currentDistance, $currentPace }) => {
    const now = Temporal.Now.instant();
    const finishMarker = event.markers.find((m) => m.kind === "finish");

    if (!finishMarker) {
      console.error("bug, finish marker wrong", finishMarker);
      return null;
    }

    const distanceToFinish = event.routeLength - ($currentDistance || 0);
    const hoursToArrival = distanceToFinish / $currentPace;
    return now.add({ seconds: Math.round(hoursToArrival * 3600) });
  });

  return store;
};

const TabView = ({ store }) => {
  const tabStore = new Livewire({
    activeTab: "stats",
  });

  const tabs = [
    { id: "stats", label: "Stats" },
    { id: "map", label: "Map" },
  ];

  return (
    <tabStore.reactive keys="activeTab">
      {({ activeTab }) => (
        <div class="flex-1 pb-16">
          {activeTab === "stats" ? (
            <StatsTab store={store} />
          ) : (
            <MapTab store={store} />
          )}
        </div>
      )}
      {({ activeTab }) => (
        <div class="fixed bottom-0 left-0 right-0 z-10">
          <div class="flex border-t border-base-300 bg-base-100">
            {tabs.map((tab) => (
              <button
                key={tab.id}
                class={`flex-1 py-3 px-4 text-center transition-all duration-200 ${activeTab === tab.id ? "bg-primary text-primary-content font-medium" : "bg-base-100 text-base-content hover:bg-base-200"}`}
                onClick={() => tabStore.reduce(() => ({ activeTab: tab.id }))}
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
    <main class="mx-auto max-w-md h-dvh flex flex-col bg-base-100">
      <TabView store={store}></TabView>
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

const StatsTab = ({ store }) => {
  return (
    <div class="flex-1 overflow-auto p-4">
      <store.reactive
        keys={[
          "$currentDistance",
          "$currentPace",
          "$requiredPace",
          "$eta",
          "event",
          "progress",
        ]}
      >
        {({ $currentDistance, $currentPace, $requiredPace, $eta, event }) => {
          const controlPoints =
            event?.markers?.filter(
              (m) => m.kind === "control" || m.kind === "finish",
            ) || [];

          return (
            <div class="space-y-6">
              <div class="stat-section">
                <div class="grid grid-cols-2 gap-4">
                  <StatCard
                    title="Distance"
                    value={`${$currentDistance.toFixed(1)} km`}
                    subtitle={getRemainingDistance(event, $currentDistance)}
                  />
                  <StatCard
                    title="Current Pace"
                    value={`${$currentPace.toFixed(1)} km/h`}
                  />
                  <StatCard
                    title="Required Pace"
                    value={`${$requiredPace.toFixed(1)} km/h`}
                  />
                  <StatCard
                    title="Finish ETA"
                    value={formatDateTimeCompact($eta)}
                    subtitle={getEtaRemainingTime($eta)}
                  />
                </div>

                <div class="grid grid-cols-2 gap-4 mt-4">
                  <StatCard
                    title="Elapsed Time"
                    value={calculateElapsedTime(event)}
                    subtitle={`Started: ${formatDateTimeCompact(event.startTime)}`}
                  />
                  <StatCard
                    title="Time Remaining"
                    value={getTimeRemaining(event.endTime)}
                    subtitle={`Cutoff: ${formatDateTimeCompact(event.endTime)}`}
                  />
                </div>
              </div>

              <div class="control-points-section">
                <div class="space-y-4">
                  {controlPoints.map((cp, index) => (
                    <ControlPointCard cp={cp} index={index} store={store} />
                  ))}
                </div>
              </div>
            </div>
          );
        }}
      </store.reactive>
    </div>
  );
};

const ControlPointCard = ({ cp, index, store }) => {
  const progressEvent = store.$.progress.find((p) => p.markerId === cp.id);
  const isCompleted = !!progressEvent;

  const handleClearCheckpoint = () => {
    const updatedProgress = store.$.progress.filter(
      (p) => p.markerId !== cp.id,
    );
    store.$.progress = updatedProgress;
  };

  const handleCheckIn = () => {
    store.$.progress = [
      ...store.$.progress,
      {
        markerId: cp.id,
        arrivalTime: Temporal.Now.instant(),
        // TODO: this needs to be calculated
        segmentPace: store.$.$currentPace || 15,
      },
    ];
  };

  return (
    <div
      key={cp.id}
      className={`card shadow-sm mb-4 overflow-hidden p-4 rounded-lg ${isCompleted ? "bg-success/10 border-l-4 border-success" : "bg-base-100 border-base-300"}`}
    >
      <div className="flex justify-between items-center mb-3">
        <div>
          <h4 className="font-bold text-lg">{cp.name || `CP ${index + 1}`}</h4>
          {cp.note && <p className="text-sm text-gray-600">{cp.note}</p>}
        </div>
        {isCompleted ? (
          <div className="flex items-center gap-2">
            <span className="badge badge-success badge-sm">Completed</span>
            <button
              className="btn btn-xs btn-ghost btn-error"
              onClick={handleClearCheckpoint}
            >
              Clear
            </button>
          </div>
        ) : (
          <span className="badge badge-warning badge-sm">Pending</span>
        )}
      </div>

      <div className="grid grid-cols-3 gap-2 text-sm mb-3">
        <div>
          <div className="text-xs text-gray-500">Distance</div>
          <div className="font-mono">
            {cp.routeDistance?.toFixed(1) || "?"} km
          </div>
        </div>
        <div>
          <div className="text-xs text-gray-500">Pace</div>
          <div className="font-mono">
            {progressEvent?.segmentPace?.toFixed(1) || "-"} km/h
          </div>
        </div>
        <div>
          <div className="text-xs text-gray-500">Time</div>
          <div className="font-mono">
            {progressEvent ? (
              <div className="flex items-center gap-1">
                <span>{formatDateTimeCompact(progressEvent.arrivalTime)}</span>
              </div>
            ) : (
              "-"
            )}
          </div>
        </div>
      </div>

      {!isCompleted && (
        <button
          className="btn btn-sm btn-primary w-full"
          onClick={handleCheckIn}
        >
          Check In Now
        </button>
      )}
    </div>
  );
};

const MapTab = ({ store }) => {
  return (
    <div class="h-full w-full relative pb-16">
      <div
        id="map-container"
        class="h-full w-full min-h-75"
        $mount={(el) => initializeMap(el, store)}
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

  map.setMarkers(markers);
  map.setTrack(allCoordinates, { fitBounds: true });
  store.watch(["userLocation"], ({ userLocation }) => {
    userLocation && map.setUserLocation(userLocation as LatLngTuple);
  });
}
