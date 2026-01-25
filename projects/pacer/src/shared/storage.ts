import { EventConfig } from "./index.js";

export async function compressToURL(obj: any): Promise<string> {
  const json = JSON.stringify(obj);
  const bytes = new TextEncoder().encode(json);

  const cs = new CompressionStream("gzip");
  const writer = cs.writable.getWriter();
  writer.write(bytes);
  writer.close();

  const compressed = await new Response(cs.readable).arrayBuffer();

  // URL-safe base64 (no +, /, or = padding)
  const base64 = btoa(String.fromCharCode(...new Uint8Array(compressed)))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");

  return base64;
}

export async function decompressFromURL(base64: string): Promise<any> {
  let padded = base64.replace(/-/g, "+").replace(/_/g, "/");
  while (padded.length % 4 !== 0) {
    padded += "=";
  }

  const binary = atob(padded);
  const bytes = Uint8Array.from(binary, (c) => c.charCodeAt(0));

  const ds = new DecompressionStream("gzip");
  const writer = ds.writable.getWriter();
  writer.write(bytes);
  writer.close();

  const json = await new Response(ds.readable).text();
  return JSON.parse(json);
}

export async function setUrlToEvent(event: EventConfig) {
  const compressed = await compressToURL(event);
  window.history.replaceState(null, "", `#r=${compressed}`);
}

/**
 * Revive Temporal.Instant objects from ISO strings after JSON deserialization
 */
function reviveTemporalInstants(obj: any): any {
  if (obj === null || typeof obj !== "object") {
    return obj;
  }

  // Handle arrays
  if (Array.isArray(obj)) {
    return obj.map((item) => reviveTemporalInstants(item));
  }

  // Handle objects
  const revived: any = {};
  for (const [key, value] of Object.entries(obj)) {
    // Check if this looks like a Temporal.Instant ISO string
    if (
      typeof value === "string" &&
      (key === "startTime" ||
        key === "endTime" ||
        key === "goalTime" ||
        key === "cutoffTime" ||
        key === "arrivalTime") &&
      /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/.test(value)
    ) {
      revived[key] = Temporal.Instant.from(value);
    } else {
      revived[key] = reviveTemporalInstants(value);
    }
  }

  return revived;
}

export async function loadRouteFromURL(): Promise<EventConfig | null> {
  const hash = window.location.hash;

  if (!hash.startsWith("#r=")) {
    return null;
  }
  try {
    const compressed = hash.slice(3);
    const data = await decompressFromURL(compressed);
    return reviveTemporalInstants(data);
  } catch (e) {
    console.error("Failed to decompress route from URL:", e);
    return null;
  }
}

export function getEventId(event: EventConfig): string {
  // TODO: betterId
  return event.name;
}

type TrackerStateSerialized = {
  state: "unstarted" | "inprogress" | "finished";
  progress: Record<
    string,
    {
      state: "unvisited" | "visited" | "skipped";
      arrivalTime?: string; // ISO string
      segmentPace?: number;
    }
  >;
};

export type TrackerState = {
  state: "unstarted" | "inprogress" | "finished";
  progress: Record<
    string,
    {
      state: "unvisited" | "visited" | "skipped";
      arrivalTime?: Temporal.Instant;
      segmentPace?: number;
    }
  >;
};

export function saveTrackerState(
  eventId: string,
  state: TrackerStateSerialized,
): void {
  try {
    const key = `tracker-state-${eventId}`;
    localStorage.setItem(key, JSON.stringify(state));
  } catch (e) {
    console.error("Failed to save tracker state:", e);
  }
}

export function loadTrackerState(eventId: string): TrackerState | null {
  try {
    const key = `tracker-state-${eventId}`;
    const data = localStorage.getItem(key);
    if (!data) return null;

    const parsed = JSON.parse(data);

    if (parsed.progress) {
      for (const markerId in parsed.progress) {
        const markerProgress = parsed.progress[markerId];
        if (markerProgress.arrivalTime) {
          markerProgress.arrivalTime = Temporal.Instant.from(
            markerProgress.arrivalTime,
          );
        }
      }
    }

    return parsed;
  } catch (e) {
    console.error("Failed to load tracker state:", e);
    return null;
  }
}
