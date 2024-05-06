import { Context, Hono } from "https://deno.land/x/hono/mod.ts";
import { html } from "https://deno.land/x/hono/helper/html/index.ts";
import { DOMParser, XMLSerializer } from "https://esm.sh/@xmldom/xmldom@0.8.2";
import { gpx as gpxToGeoJSON } from "https://unpkg.com/@tmcw/togeojson?module";
import { copy } from "jsr:@std/io/copy";

const STORE = new Map<string, string | symbol>();
const RESERVED = Symbol("reserved");

function nextRouteId(): string {
  while (true) {
    const id = Math.round(Math.random() * 10_000).toString();

    if (!Object.hasOwn(STORE, id)) {
      STORE[id] = RESERVED;
      // Ensure the route gets freed eventually.
      setTimeout(() => delete STORE[id], 20 * 60 * 1_000);
      return id;
    }
  }
}

function renderUploadPage(routeId: string) {
  return html`
    <head>
      <title>Upload a route</title>
    </head>

    <body>
      <form
        enctype="multipart/form-data"
        action="/routes/${routeId}/upload"
        method="post"
      >
        <label for="file">Route</label>
        <input name="file" type="file" accept=".gpx" />
        <br />
        <button>Upload</button>
      </form>
    </body>
  `;
}

async function geojson2fit(geojson: string): Promise<ArrayBuffer> {
  const command = new Deno.Command("geojson2fit/geojson2fit", {
    stdin: "piped",
    stdout: "piped",
    stderr: "piped",
  });

  const child = command.spawn();
  const writer = child.stdin.getWriter();
  await writer.write(new TextEncoder().encode(geojson));
  writer.releaseLock();

  await child.stdin.close();

  const { code, stdout, stderr } = await child.output();
  console.assert(code === 0, new TextDecoder().decode(stderr));
  return stdout;
}

async function gpx2geojson(gpx: File): Promise<any> {
  const buf = await gpx.arrayBuffer();
  const text = new TextDecoder().decode(buf);
  const xml = new DOMParser().parseFromString(text);

  return gpxToGeoJSON(xml);
}

const app = new Hono();

app.get("/", (c: Context) => c.text("Hello Deno!"));
app.get("/api/upload_url", (c: Context) => {
  const baseUrl = c.req.url.replace(c.req.routePath, "");
  const routeId = nextRouteId();

  return c.json({
    upload_url: `${baseUrl}/routes/${routeId}`,
    download_url: `${baseUrl}/routes/${routeId}/fit`,
  });
});

app.get("/routes/:routeId", (c: Context) => {
  const routeId = c.req.param("routeId");
  return c.html(renderUploadPage(routeId));
});

app.post("/routes/:routeId/upload", async (c: Context) => {
  const routeId = c.req.param("routeId");
  const body = await c.req.parseBody();
  const file = body["file"] as File;

  const geojson = await gpx2geojson(file);
  STORE.set(routeId, JSON.stringify(geojson));

  return c.text("Stored, return to Garmin.");
});

app.get("/routes/:routeId/:fmt", async (c: Context) => {
  const routeId = c.req.param("routeId");
  const fmt = c.req.param("fmt");
  const route = STORE.get(routeId);

  if (route == null) return c.text("not found", 404);
  if (route === RESERVED) return c.text("not uploaded", 401);
  if (fmt !== "fit") return c.text("unsupported", 400);

  const fit = await geojson2fit(route);

  c.status(200);
  c.header("Content-Type", "application/octet-stream");
  return c.body(fit);
});

Deno.serve({ port: 8080, hostname: "0.0.0.0" }, app.fetch);
