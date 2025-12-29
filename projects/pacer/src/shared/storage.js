/**
 * Compress an object to a URL-safe string.
 * @param {Object} obj
 * @returns {Promise<string>}
 */
export async function compressToURL(obj) {
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

/**
 * Decompress a URL-safe string back to an object.
 * @param {string} base64
 * @returns {Promise<Object>}
 */
export async function decompressFromURL(base64) {
  // Restore standard base64
  let padded = base64.replace(/-/g, "+").replace(/_/g, "/");
  // Add padding if needed
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

export async function saveRouteToURL(route) {
  const compressed = await compressToURL(route);
  window.history.replaceState(null, "", `#r=${compressed}`);
}

export async function loadRouteFromURL() {
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

function getStorageKey(id) {
  return `route.${id}`;
}

export function saveState(id, state) {
  if (!id) return;
  const key = getStorageKey(id);
  localStorage.setItem(key, JSON.stringify(state));
}

export function restoreState(id) {
  const key = getStorageKey(id);
  const json = localStorage.getItem(key);
  if (!json) return null;

  try {
    return JSON.parse(json);
  } catch (e) {
    console.error("Failed to parse tracking data:", e);
    return null;
  }
}
