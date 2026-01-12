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

export async function loadRouteFromURL(): Promise<EventConfig | null> {
  const hash = window.location.hash;

  if (!hash.startsWith("#r=")) {
    return null;
  }

  try {
    const compressed = hash.slice(3);
    return await decompressFromURL(compressed);
  } catch (e) {
    console.error("Failed to decompress route from URL:", e);
    return null;
  }
}
