# Sorelegs — Complete Application Specification

A client-side pace tracker for multi-day endurance cycling events. Riders upload
a GPX route, place checkpoints with cutoff and goal times, then track their live
position against those constraints during the ride. No server. Everything runs in
the browser, persists to localStorage, and can be shared via URL.

---

## 1. Technology Stack

| Concern | Choice |
|---|---|
| Rendering / reactivity | **Livewire** — a custom, zero-dependency JSX-to-DOM library (described fully in §11) |
| Mapping | **Leaflet** with Stadia Maps raster tiles (dark: `alidade_smooth_dark`, light: `alidade_smooth`) |
| Geospatial math | **Turf.js** — `nearest-point-on-line`, `line-slice`, `length`, `helpers` |
| GPX parsing | **@tmcw/togeojson** (XML → GeoJSON) |
| Date/time | **Temporal API** via `temporal-polyfill/global`. All timestamps are `Temporal.Instant`. |
| CSS | **Tailwind CSS 4** + **DaisyUI 5**. Three themes: `light`, `dark`, `halloween` (default dark). |
| Build | **Vite** (rolldown-vite). Custom plugin auto-injects Livewire JSX pragma into `.tsx`/`.jsx` files. OXC classic JSX mode. |
| Language | TypeScript (strict, ES2024 target) |

All coordinates follow the **GeoJSON convention**: `[longitude, latitude]`.

---

## 2. Data Model

### 2.1 `Meters` (branded type)

```ts
type Meters = number & { readonly __brand: "m" };
```

Constructor: `Meters(value: number)`. Conversions: `metersToKm(m)`, `kmToMeters(km)`.

### 2.2 `EventConfig`

The top-level object describing a race event.

```ts
type EventConfig = {
  id: string;              // random base-36 string
  name: string;            // human-readable event name
  notes?: string;          // optional free-text
  startTime: Temporal.Instant;
  endTime: Temporal.Instant;  // overall event cutoff
  routeLength: Meters;        // sum of all segment lengths
  segments: Segment[];
  markers: RouteMarker[];
};
```

### 2.3 `Segment`

One continuous GPS track (one GPX `<trk>`). A route may consist of multiple
segments (multiple GPX files or MultiLineString tracks split apart).

```ts
type Segment = {
  id: string;
  title: string;           // track name from GPX, or filename
  fileName: string;        // source GPX filename
  segmentLength: Meters;
  geometry: GeoJSON.LineString;  // coordinates: [lng, lat][]
};
```

### 2.4 `RouteMarker`

A point of interest along the route — start line, finish line, intermediate
checkpoint, or timed control.

```ts
type RouteMarker = {
  id: string;
  kind: "start" | "finish" | "marker" | "control";
  name?: string;
  note?: string;
  icon?: string;
  segmentId?: string;         // which segment this marker is snapped to
  routeDistance?: Meters;     // cumulative distance from route start
  goalTime?: Temporal.Instant;    // soft target arrival time
  cutoffTime?: Temporal.Instant;  // hard deadline arrival time
  coordinate: [number, number];   // [lng, lat]
};
```

**Kind semantics:**
- `start` — the ride begins here. Always first. Shows event start time.
- `finish` — the ride ends here. Always last. Has the event's overall cutoff.
- `marker` — a waypoint with an optional goal time (soft target).
- `control` — a timed checkpoint with a cutoff time (hard deadline). Missing the
  cutoff means disqualification.

### 2.5 `TrackerState`

Progress state for a specific event, stored separately from the event config.

```ts
type TrackerState = {
  state: "unstarted" | "inprogress" | "finished";
  progress: Record<string, MarkerVisitStatus>;
};

type MarkerVisitStatus = {
  state: "unvisited" | "visited" | "skipped";
  arrivalTime?: Temporal.Instant;   // when the rider reached this marker
  segmentPace?: number;             // km/h at the time of arrival
};
```

---

## 3. Storage Architecture

### 3.1 localStorage (source of truth)

Two independent stores:

| Key | Contents |
|---|---|
| `pacer-event` | The `EventConfig` — route, markers, timing. |
| `tracker-state-{eventId}` | The `TrackerState` — visit progress for one event. |

All Temporal.Instant values serialize as ISO-8601 strings via `JSON.stringify`.
On load, a recursive `reviveTemporalInstants()` function walks the parsed JSON
and converts strings back to `Temporal.Instant` for keys named: `startTime`,
`endTime`, `goalTime`, `cutoffTime`, `arrivalTime`.

### 3.2 URL hash (share/import wire format)

Events are shared via a URL of the form:

```
https://host/path#r={payload}
```

**Encoding** (`compressToURL`):
1. `JSON.stringify` the EventConfig.
2. Encode to UTF-8 bytes.
3. Compress with `CompressionStream("gzip")`.
4. Encode to base64 with URL-safe alphabet: `+` → `-`, `/` → `_`, strip `=` padding.

**Decoding** (`decompressFromURL`): Reverse the above.

**Import flow** (on page load):
1. Check `window.location.hash` for `#r=` prefix.
2. Decompress and revive Temporal instants.
3. Save the imported event to localStorage.
4. Clear the hash from the URL via `history.replaceState`.
5. Return the event for immediate use.

### 3.3 Share action

Compress current event → build full URL → copy to clipboard. Should display a
brief toast or visual confirmation that the URL was copied.

---

## 4. Application Modes and Initialization

### 4.1 Global state

```ts
type GlobalStoreProps = {
  mode: "SETUP" | "PACE_TRACKER";
  units: "METRIC" | "IMPERIAL";   // metric only for now
  darkmode: boolean;               // true = halloween theme
};
```

### 4.2 Init sequence

```
1. await importEventFromURL()  // check for #r= hash import
2. loadEventFromLocalStorage() // check localStorage
3. If event exists → mode = PACE_TRACKER
   If no event   → mode = SETUP
```

### 4.3 Mode transitions

```
SETUP  ──(Done button)──→  PACE_TRACKER
                            (saves event to localStorage, keeps event ID on edit)

PACE_TRACKER  ──(Edit tab)──→  SETUP
                                (passes current event for editing)
```

---

## 5. Global Shell (Navbar)

Fixed navbar at the top of the viewport. Max width constrained (`max-w-5xl`,
centered).

**Left:** Brand text "sorelegs" (styled as ghost button).

**Right:**
- "Clear Storage" button — calls `localStorage.clear()` and reloads the page.
  This is the nuclear reset: clears event config and all tracker progress.
- Theme toggle button — moon SVG icon. Toggles `darkmode` boolean. A watcher
  sets `data-theme` on `<html>` to either `"halloween"` (dark) or `"light"`.

Below the navbar, the mode-specific view fills the remaining viewport.

---

## 6. Setup View

**Purpose:** Create or edit an `EventConfig`.

**Local store:**

```ts
type StoreProps = {
  trackName: string;
  startTime?: Temporal.Instant;
  endTime?: Temporal.Instant;
  segments: Segment[];
  markers: RouteMarker[];
};
// Computed:
$valid = trackName.length > 0 && !!startTime && !!endTime
```

When editing an existing event, the store initializes from that event. When
creating new, the store starts empty (no segments, no markers, no times).

**Layout:** Single scrollable column. On desktop (`md:` breakpoint), the
fieldsets and map form a 2-column grid.

### 6.1 Timing fieldset

Two `<input type="datetime-local">` fields: Start and End.

Validation on blur: if empty, sets `aria-invalid="true"` on the input. Removing
invalidity once a value is entered.

Values convert between `Temporal.Instant` and the HTML `datetime-local` string
format using the user's local timezone (`Temporal.Now.timeZoneId()`).

### 6.2 Route fieldset

Hidden `<input type="file" accept=".gpx" multiple>`. Triggered by an "Add Route
Files" button.

Summary line (reactive): `"{N} segments"` badge and `"{X}km"` badge showing
total distance across all segments.

Segment list: Each row shows title (or filename fallback), filename badge,
distance badge (km, rounded to integer), and a remove (×) button. Removing a
segment that has markers snapped to it shows a `confirm()` dialog asking whether
to also remove those markers.

### 6.3 GPX processing pipeline

For each uploaded file:

1. Read as text via `File.text()`.
2. Parse with `parseGPX()` → GeoJSON tracks (LineString features) and markers
   (Point features).
3. For each track:
   - MultiLineString features are split into individual LineString features.
   - Create a `Segment` with a generated ID, the track name, the source
     filename, length measured via `turf/length`, and the geometry.
4. For each waypoint:
   - Snap to the nearest track via `turf/nearest-point-on-line`.
   - Record the `routeDistance` (distance along that track to the snapped point)
     and the `segmentId` of the nearest track.
   - Create a `RouteMarker` with kind `"marker"`.

After processing, new segments are appended to the store. New markers are merged
with existing markers, then the full list is sorted by `routeDistance` (markers
without a distance preserve their relative position among themselves).

The file input is reset after processing so the same file can be re-added.

### 6.4 Markers fieldset

Reactive list of `RouteMarkerRow` components, one per marker in order.

Each row shows:
- Marker name (or "CP {index+1}" fallback)
- Segment badge: title of the segment this marker is snapped to, or "Unattached"
- Dropdown menu ("...") with actions

**Marker dropdown menu options:**
1. **Edit name** — reveals inline text input + check button
2. **Edit note** — reveals inline textarea + check button
3. *separator*
4. **Clear timing** — removes both goalTime and cutoffTime, resets kind to "marker" (only shown if marker has timing)
5. **Set cutoff time** — reveals inline datetime-local input + check button. Shows current value as badge if set.
6. **Set goal time** — same as cutoff.
7. *separator*
8. **Remove marker** — deletes from the markers array

Below the name row: note text (italic, if set), then badges for route distance
(km), goal time, and cutoff time.

### 6.5 Map fieldset

Interactive Leaflet map in a fixed-height container (`h-100`). Shows:
- All track segments as blue polylines
- All markers as small colored dots with tooltip showing name

**Map interactions:**

- **Fit bounds:** On initial load and whenever segments are added/removed, the
  map fits its bounds to show all content with padding.
- **Click on map:** Opens a Leaflet popup asking "Add control here?" with an Add
  button. The clicked point is snapped to the nearest track segment within 200px
  (screen distance). The new marker is added and the list re-sorted.
- **Drag a marker:** Markers are draggable. On drag end, the marker is snapped
  to the nearest track segment within 50px. The marker's coordinate, segmentId,
  and routeDistance are updated. The markers list is re-sorted.
- **Dark mode:** The map tile layer switches when the global darkmode changes.

The map watcher auto-cleans-up if the map container is detached from the DOM.

### 6.6 Done button

Enabled only when `$valid` is true. Assembles an `EventConfig`:
- Uses the existing event's `id` if editing, otherwise generates a new one.
- `routeLength` = sum of all segment lengths.
- Calls `onSetupComplete(eventConfig)`, which saves to localStorage and
  transitions to PACE_TRACKER mode.

---

## 7. Pace Tracker View

**Purpose:** Live tracking during the event, showing stats and map.

### 7.1 Tracker store

```ts
type StoreProps = {
  state: "unstarted" | "inprogress" | "finished";
  event: EventConfig;
  progress: Record<string, MarkerVisitStatus>;
  userLocation?: [number, number];  // [lng, lat]
};

// Computed:
$currentDistance: Meters   // distance along route from start
$currentPace: number       // km/h overall
```

Initialization:
- Loads saved `TrackerState` from `tracker-state-{eventId}` if available.
- Defaults to `state: "inprogress"` and empty progress.
- Starts GPS watch for live location updates.
- Sets up state persistence.

### 7.2 Computed: `$currentDistance`

Given a `userLocation`:
1. Use `calculateRoutePosition()` to snap the user position to the track and
   measure distance from start.
2. If the user is more than 50 meters from the track, return 0 (off-route).
3. Operates across all segments of the route (cumulative distance).

Returns `Meters(0)` if no location is available.

### 7.3 Computed: `$currentPace`

```
pace = distanceKm / hoursElapsed
```

Where `hoursElapsed` is time since `event.startTime`. Returns 0 if distance is
0 or elapsed time is negligible (< 0.01 hours).

### 7.4 Auto-checkpoint detection

A watcher on `$currentDistance` iterates all non-start markers:
- If `$currentDistance >= marker.routeDistance` and the marker is not yet visited:
  - Mark as `visited` with current timestamp and current pace.
- Updates `store.$.progress` to trigger persistence.

### 7.5 State persistence

A watcher on `["state", "progress"]` serializes the TrackerState to localStorage
on every change. Temporal.Instant values in `arrivalTime` are serialized via
`.toString()` and deserialized via `Temporal.Instant.from()`.

### 7.6 GPS tracking

Use `navigator.geolocation.watchPosition` for continuous location updates:
- On each position, set `store.$.userLocation = [longitude, latitude]`.
- Handle errors gracefully (log, don't crash).

For development: a mock function replays the first segment's coordinates at
regular intervals (total duration ~30 seconds across all points, minimum 250ms
per step). Guarded by a global flag to prevent duplicates.

### 7.7 Tab bar

Fixed at the bottom of the viewport. Full width. Four items:

| Tab | Behavior |
|---|---|
| **Stats** | Shows the stats tab (default active) |
| **Map** | Shows the map tab |
| **Edit** | Switches global mode to SETUP (passes current event for editing) |
| **Share** | Compresses event → copies URL to clipboard |

Active tab is highlighted with primary color. Edit and Share behave as actions,
not true tab switches.

---

## 8. Stats Tab

### 8.1 Stat cards grid

A 2-column grid of `StatCard` components. Each card has a title (small, muted),
a value (large, bold), and an optional subtitle (small, gray).

**Cards shown:**

1. **Distance**
   - Value: `"{X.X} km"` — current distance along route
   - Subtitle: `"{X.X} km remaining"` — distance to finish

2. **Overall Pace**
   - Value: `"{X.X} km/h"` — current pace

3. **Min Pace ({next checkpoint name})** *(conditional)*
   - Only shown if there's an unvisited marker with a cutoff time ahead.
   - Value: the required pace (km/h) to reach that checkpoint before its cutoff.
   - Subtitle: `"Cutoff: {formatted cutoff time}"`

4. **ETA ({next checkpoint name})** *(conditional, paired with #3)*
   - Value: relative time to ETA (`formatRelativeTime`)
   - Subtitle: absolute ETA (`formatDateTimeCompact`)

5. **Finish ETA**
   - Value: relative time to projected finish
   - Subtitle: absolute projected finish time

6. **Pacing**
   - Value: formatted duration of the pacing delta
   - Subtitle: `"ahead"` or `"behind"` schedule
   - Pacing delta = `event.endTime.since(finishEta)`. Positive means ahead.

7. **Elapsed Time**
   - Value: relative time since start
   - Subtitle: `"Started: {formatted start time}"`

8. **Time Remaining**
   - Value: relative time until event end
   - Subtitle: `"Finish Cutoff: {formatted end time}"`

### 8.2 Route marker cards

Below the stat grid: a list of `RouteMarkerCard` components, one per marker.

Each card has three visual states:

#### Start point (kind = "start")
- Dimmed background (`bg-gray-600/10`)
- Shows a single stat: "Start Time" with the formatted event start time

#### Completed checkpoint (visited in progress)
- Dimmed background
- Header: editable name + Reset button (clears visit status)
- Stats row (3-col grid): Distance (km), Arrival Pace (km/h), Arrival Time

#### Upcoming checkpoint (not yet visited, not start)
- Active background with border
- Header: editable name + next-time badge (see below)
- Note text if set
- Stats (3-col grid, wrapping):
  - **Distance** — km remaining to this marker
  - **ETA** — relative time to estimated arrival
  - **Cutoff** — tappable, shows formatted cutoff time or "Set..."
  - **Goal** — tappable, shows formatted goal time or "Set..."
  - **Min Pace** — required pace to reach cutoff (conditional)
  - **Goal Pace** — required pace to reach goal (conditional)

**Next-time badge logic** (`getNextRelevantTime`): Determines which time
(cutoff or goal) is most relevant to show as a badge next to the marker name.
Priority:
- If neither exists: no badge
- If only one exists: show that one
- If both exist and neither has passed: show the earlier one
- If both exist and both have passed: show the later one
- If one has passed and one hasn't: show the one that hasn't passed

**Inline editing:**

Each card maintains its own `{ editing: string | null }` state. Three tappable
fields:

1. **Name** — tap the marker name to reveal a text input. On blur, saves the new
   name via `updateMarker()` (patches the marker in the event config and persists
   to localStorage).
2. **Cutoff time** — tap the cutoff display to reveal a `datetime-local` input.
   On blur, saves.
3. **Goal time** — same as cutoff.

---

## 9. Map Tab

Full-height Leaflet map within the tracker. Shows:

- **Track polylines** — blue, weight 4, opacity 0.7. Each segment gets a popup
  with its title.
- **Route markers** — small circles with primary color, tooltip with name.
  Expand on hover.
- **User location** — pulsing blue dot (animated, 12×12px). Created on first
  location update, repositioned on subsequent updates.
- **Recenter control** — top-left custom control button (focus/crosshair icon).
  Fits the map bounds to show all track segments and markers with 100px padding.

The map watches:
- `userLocation` — moves the blue dot
- `darkmode` — switches tile URL between dark and light variants

---

## 10. Calculations

### 10.1 Route position (`calculateRoutePosition`)

Given a track's coordinates and a user position:
1. Find the nearest point on the line (`turf/nearest-point-on-line`).
2. Slice the line from the start to the nearest point (`turf/line-slice`).
3. Measure the slice length (`turf/length`) → `distanceFromStart`.
4. Also return `distanceFromTrack` (perpendicular distance from user to route).

### 10.2 Snap to track (`snapToNearestTrackSegment`)

Given a click/drag location in `[lng, lat]`:
1. For each segment, find the nearest point on its track.
2. Convert both the click point and snapped point to pixel coordinates via
   Leaflet's `latLngToLayerPoint`.
3. Compute pixel distance between them.
4. Keep the result with the smallest pixel distance, if within the threshold.
5. Return `{ coord, meters, segmentId, pixelDistance }`.

Threshold: 50px for marker drag snapping, 200px for map-click marker creation.

### 10.3 Required pace

```
requiredPace = remainingDistanceKm / remainingTimeHours
```

Returns null if the target time has already passed or the distance is already
covered.

### 10.4 ETA

```
eta = now + (remainingDistanceKm / currentPace) * 3600 seconds
```

Returns null if pace ≤ 0 or remaining distance ≤ 0.

### 10.5 Track length

```
length = turf/length(lineString(coordinates), { units: "kilometers" })
```

---

## 11. Livewire Reactivity System

A custom, zero-dependency library for reactive UI. No virtual DOM. JSX compiles
directly to `document.createElement` calls that produce real DOM nodes.

### 11.1 Vite integration

A custom Vite plugin (`livewireJsxPlugin`) prepends this import to every
`.jsx`/`.tsx` file:

```ts
import { createElement, createFragment } from '@/livewire';
```

OXC JSX config uses classic mode with `pragma: "createElement"` and
`pragmaFrag: "createFragment"`.

### 11.2 `createElement(tag, attrs, ...children)`

- If `tag` is a function: call it as a component, passing `attrs` and children.
- If `tag` is `"fragment"`: return a `DocumentFragment`.
- Otherwise: create an `HTMLElement`.

**Attribute handling:**
- `$mount`: queues a microtask that calls the function with the element (lifecycle hook for initialization like Leaflet maps).
- `on*` attributes: `addEventListener(eventName.toLowerCase(), handler)`.
- `className` → `setAttribute("class", ...)`.
- `innerHTML` → direct property set.
- `style` object → `setProperty` for each entry.
- `false`, `""`, `null`, `undefined` → `removeAttribute`.
- Everything else → `setAttribute(key, String(value))`.

**Children:** Flattened arbitrarily deep. Each child is converted via `toDOMNode`:
- `Node` → used directly
- `function` → called, result converted recursively
- `null`, `false`, `[]` → skipped
- Everything else → `createTextNode(String(val))`

### 11.3 `Livewire<Props, Computed>` class

```ts
const store = new Livewire<{ count: number }, { $double: number }>({
  count: 0,
});
store.compute("$double", ({ count }) => count * 2);
```

**`store.$`** — Proxy for reading/writing state.
- Setting a property queues a microtask tick.
- Setting a computed property throws.
- Setting an unknown property throws.
- Setting to the same value (reference equality) is a no-op.

**`store.compute(key, fn)`** — Registers a derived property. Recomputed every
tick. The key must start with `$`.

**`store.update(key, fn)`** — Functional update: `fn(currentValue)` → new value.

**`store.watch(keys, fn)`** — Registers an observer. `keys` is a string or
array of property names. The observer fires on each tick, but only if the
JSON serialization of the watched keys has changed since the last call.
Returns an unsubscribe function.

**`store.watch(fn)`** — Registers an observer that fires on every tick
(no filtering).

**Tick cycle:** `queueMicrotask` → recompute all computed properties → notify
all observers.

### 11.4 Reactive rendering

**`store.render(keys, fn)`** — Returns a `DocumentFragment` containing:
1. An anchor `Comment` node (`<!-- render(keys) -->`)
2. The initial render output from `fn(state)`

On each tick where the watched keys change, the previous DOM nodes are removed
and `fn(state)` is called again to produce new nodes, inserted after the anchor.

If the anchor comment is no longer connected to the DOM, the watcher
auto-unsubscribes (cleanup).

**`<store.reactive keys={...}>`** — JSX sugar for `render()`. Children are
render functions that receive state.

**`<store.reactiveEach key="propName">`** — Maps over an array property.
Children are `(value, index, state) => Node` functions.

### 11.5 `htmlTemplate`

Tagged template literal that creates a `DocumentFragment` from raw HTML via a
`<template>` element.

---

## 12. GPX Parsing

Uses `@tmcw/togeojson` to convert GPX XML to GeoJSON FeatureCollection.

**Input:** Raw GPX XML string.

**Processing:**
- `LineString` features → collected as tracks
- `MultiLineString` features → split into individual `LineString` features,
  each named `"{original name} ({index})"`.
- `Point` features → collected as markers with properties: `name`,
  `note` (from `desc` or `cmt`), `icon` (from `sym`).

**Output:** `{ tracks: Feature<LineString>[], markers: Feature<Point>[] }`

---

## 13. Time Formatting

All time display uses the user's local timezone via `Temporal.Now.timeZoneId()`.

| Function | Format | Example |
|---|---|---|
| `formatDateTimeCompact(instant)` | `"HH:MM Mon D"` | `"14:30 Feb 1"` |
| `formatDuration(duration)` | `"Xd Xh Xm"` | `"2d 5h 30m"` |
| `formatRelativeTime(instant)` | duration between now and instant | `"1d 3h 15m"` |
| `instantToDateTimeLocal(instant)` | ISO datetime-local for `<input>` | `"2026-03-01T14:30"` |
| `dateTimeLocalToInstant(string)` | parse datetime-local in local tz | → `Temporal.Instant` |

`formatDuration` returns `"n/a"` if all components are zero.

`formatRelativeTime` returns `"??"` for null input.

`formatDateTimeCompact` returns `"--"` for null input.

---

## 14. Map Controller

Wraps Leaflet in a `MapController` class.

### 14.1 `createMap(container, options)`

Creates a Leaflet map with:
- Default center: `[52.52, 13.405]` (Berlin), zoom 6
- Stadia Maps tile layer (dark by default)
- OpenStreetMap attribution
- Max zoom: 19
- Recenter control (top-left)

Returns a `MapController` instance.

### 14.2 MapController API

| Method | Behavior |
|---|---|
| `setTrackSegments(segments, options)` | Clears previous polylines, draws new ones. Options: `fitBounds`, `color` (default `#2563eb`), `weight` (4), `opacity` (0.7). Each segment gets a popup with its title. |
| `setRouteMarkers(markers, options)` | Clears previous markers, places new ones. Each marker is a `divIcon` — a small circle with primary color, tooltip, hover-expand animation. Options: `onClick(marker)`, `onDrag(index, latlng, leafletMarker)`. Drag is enabled if `onDrag` is provided. |
| `setUserLocation(latlng)` | Creates or moves a pulsing blue dot marker (12×12px `divIcon` with Tailwind classes). |
| `fitToContent()` | Fits map bounds to all track polylines and markers with 100px padding. |
| `setDarkMode(boolean)` | Switches tile URL between dark and light variants. |
| `onMapClick(callback)` | Registers a click handler on the map. |
| `clearMarkers()` | Removes all route markers and control segments from the map. |

### 14.3 Marker rendering

Route markers use a `divIcon`:
```html
<div class="w-3 h-3 -translate-1/2 rounded-full bg-primary/50 tooltip
  hover:w-6 hover:h-6 transition-all border-2 border-primary drop-shadow-2xl"
  data-tip="{name}">
</div>
```

User location marker:
```html
<div class="w-3 h-3 rounded-full bg-blue-600 border-2 border-white animate-pulse">
</div>
```

### 14.4 Dashed control segments (intended, not yet implemented)

Between consecutive checkpoints, if either is not snapped to a track segment,
draw a dashed line connecting them:
```
color: "#666", weight: 2, opacity: 0.7, dashArray: "5, 10"
```

---

## 15. Styling

### 15.1 CSS structure

```css
@import "tailwindcss";
@plugin "daisyui" {
    themes: light --default, dark, halloween --prefersdark;
}
```

### 15.2 Leaflet overrides

- `.leaflet-container`: full height/width
- `.leaflet-marker-icon`: no transitions (prevents jumping during drag)
- `.leaflet-div-icon`: margin 0, centered via `translate(-50%, -50%)`
- `.leaflet-marker-pane`: z-index 600
- `.user-location-marker`: pulse animation
- `#map-container`: full height/width

### 15.3 HTML shell

```html
<!doctype html>
<html lang="en" data-theme="halloween">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="icon" ... /> <!-- bicycle emoji SVG favicon -->
    <title>Pace Planner</title>
  </head>
  <body>
    <div id="app"></div>
    <script type="module" src="/src/main.js"></script>
  </body>
</html>
```

---

## 16. Marker Sorting Algorithm

When markers are added or moved, the full list is re-sorted:

1. Partition into two groups: markers with `routeDistance` (snapped) and without
   (unsnapped).
2. Sort the snapped group by `routeDistance` ascending.
3. Merge back: unsnapped markers retain their original index positions in the
   combined list; snapped markers fill the remaining slots in sorted order.

This ensures snapped markers appear in geographic order while unsnapped markers
don't jump around unexpectedly.

---

## 17. ID Generation

```ts
const generateId = () => (1e16 * Math.random()).toString(36);
```

Produces a ~10-character base-36 string. Used for event IDs, segment IDs, and
marker IDs.

---

## 18. Build Configuration

### 18.1 Vite config

```ts
export default defineConfig({
  resolve: {
    alias: [{ find: "@", replacement: "/src" }],
  },
  plugins: [tailwindcss(), livewireJsxPlugin()],
  oxc: {
    jsx: {
      runtime: "classic",
      development: false,
      pragma: "createElement",
      pragmaFrag: "createFragment",
    },
  },
});
```

### 18.2 TypeScript config

- Target: ES2024
- Module: NodeNext
- JSX: preserve (Vite handles compilation)
- Strict mode with `noImplicitAny`
- Libraries: DOM, ES2024

### 18.3 Dependencies

**Runtime:**
- `@tmcw/togeojson` ^6.0.1
- `@turf/helpers`, `@turf/length`, `@turf/line-slice`, `@turf/nearest-point-on-line` (all ^7.3.1)
- `@turf/along`, `@turf/bbox`, `@turf/distance`, `@turf/line-slice-along`, `@turf/simplify` (available but not all used)
- `leaflet` ^1.9.4 + `@types/leaflet`
- `temporal-polyfill` ^0.3.0

**Dev:**
- `tailwindcss` ^4.1, `@tailwindcss/vite`, `@tailwindcss/typography`
- `daisyui` ^5.5
- `typescript` ^5.9
- `vite` via `rolldown-vite` ^7.2.5

---

## 19. Intended Features Not Yet Implemented

### GPS and Location
- **Live GPS tracking.** `watchUserLocation()` needs to be wired to
  `navigator.geolocation.watchPosition` for continuous position updates during
  the ride.
- **Multi-segment distance accumulation.** Distance calculation should work
  across all segments of the route, not just the first. When the route spans
  multiple GPX files, the cumulative distance must account for the prior
  segments' total length.

### Setup
- **Auto-generate start/finish markers.** When a route is loaded, the first
  point of the first segment should become a `"start"` marker and the last point
  of the last segment should become a `"finish"` marker (with the event's end
  time as its cutoff). The user can rename them but they shouldn't need to
  manually create them.
- **Empty state for new events.** A fresh "Create Event" flow should start with
  blank fields, not demo data. The demo data is useful for development but
  shouldn't be the production default.

### Tracker
- **Periodic stat refresh.** Elapsed time, time remaining, ETAs, and pacing
  values reference `Temporal.Now.instant()` at render time, but only re-render
  when `userLocation` changes. A periodic timer (every ~30-60 seconds) should
  trigger a re-render of time-dependent stats even when the user isn't moving.
- **Map progress visualization.** The tracker map should visually indicate which
  checkpoints have been visited (different color/opacity), the user's current
  position on the route, and ideally the completed portion of the route as a
  highlighted polyline segment.
- **Share confirmation.** After copying the share URL to clipboard, display a
  brief toast/notification confirming success.
- **Dashed control segments on map.** Between consecutive markers, if either is
  not snapped to a track, draw a dashed connector line (partially implemented in
  MapController as a commented-out TODO).

### Data Integrity
- **Tracker state cleanup.** When a new event replaces an old one,
  `tracker-state-{oldEventId}` should be removed from localStorage.
- **URL import confirmation.** When importing via URL hash while an existing
  event is already saved, prompt the user or show a warning before overwriting.
- **Progress reconciliation.** When markers are edited in setup and the user
  returns to the tracker, orphaned progress entries (for deleted/changed marker
  IDs) should be cleaned up.

### Polish
- **Offline tile caching.** Service worker to cache map tiles for use during the
  ride when connectivity is poor.
- **Imperial units.** The `units` property exists in the global store but isn't
  wired to any display logic. All display currently uses metric (km, km/h).
