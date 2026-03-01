import { EventConfig } from "./index.js";

const EVENT_STORAGE_KEY = "pacer-event";

export function saveEventToLocalStorage(event: EventConfig): void {
  saveToLocalStorage(EVENT_STORAGE_KEY, event);
}

export function loadEventFromLocalStorage(): EventConfig | null {
  const data = loadFromLocalStorage<any>(EVENT_STORAGE_KEY);
  if (!data) return null;
  return reviveTemporalInstants(data);
}

export async function generateShareURL(event: EventConfig): Promise<string> {
  const compressed = await compressToURL(event);
  return `${window.location.origin}${window.location.pathname}#r=${compressed}`;
}

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

function reviveTemporalInstants(obj: any): any {
  if (obj === null || typeof obj !== "object") {
    return obj;
  }

  if (Array.isArray(obj)) {
    return obj.map((item) => reviveTemporalInstants(item));
  }

  const revived: any = {};
  for (const [key, value] of Object.entries(obj)) {
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

export async function importEventFromURL(): Promise<EventConfig | null> {
  const hash = window.location.hash;

  if (!hash.startsWith("#r=")) {
    return null;
  }
  try {
    const compressed = hash.slice(3);
    const data = await decompressFromURL(compressed);
    const event = reviveTemporalInstants(data) as EventConfig;

    // Import into localStorage and clear the URL hash
    saveEventToLocalStorage(event);
    window.history.replaceState(null, "", window.location.pathname);

    return event;
  } catch (e) {
    console.error("Failed to import event from URL:", e);
    return null;
  }
}

export function saveToLocalStorage<T>(key: string, data: T): void {
  try {
    localStorage.setItem(key, JSON.stringify(data));
  } catch (e) {
    console.error(`Failed to save to localStorage [${key}]:`, e);
  }
}

export function loadFromLocalStorage<T>(key: string): T | null {
  try {
    const data = localStorage.getItem(key);
    return data ? JSON.parse(data) : null;
  } catch (e) {
    console.error(`Failed to load from localStorage [${key}]:`, e);
    return null;
  }
}
