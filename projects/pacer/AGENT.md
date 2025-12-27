
# Pacer - Endurance Cycling Tracker: Development Handoff

## Project Overview

### Original Motivation
The user had a simple static HTML page (`tcrpace.html`) for tracking progress during the Transcontinental Race (TCR). It worked well but had limitations:
- Hardcoded checkpoints and distances
- Required manual check-in at each checkpoint
- No flexibility for different events
- Not shareable between riders

**Goal:** Generalize this into a reusable tool for any endurance cycling event.

### Core Constraints

1. **No Backend Required** - Must work as a pure client-side application
   - Can be hosted on any static host (GitHub Pages, Netlify, etc.)
   - No server costs, no maintenance
   
2. **URL-Based Sharing** - Route configuration stored in URL fragment
   - Compressed with native `CompressionStream` API (gzip)
   - Users bookmark the URL or share with others
   - iOS can handle large URL fragments without issue

3. **LocalStorage for Personal Data** - Tracking data (check-in times) separate from route
   - Keyed by route ID (creation timestamp)
   - Personal to each device
   - Won't break if route is edited (uses checkpoint IDs, not indices)

4. **Dependency Lite** - Minimal external dependencies
   - Turf.js for geospatial operations (essential)
   - Leaflet for maps (essential)
   - Native browser APIs where possible
   - No frontend framework (vanilla JS)

5. **Mobile-First** - Optimized for use during events
   - Information dense, minimal scrolling
   - Touch-friendly
   - Works offline once loaded
   - Responsive design

## Functional Requirements

### Two Modes

#### 1. Setup Mode (Route Configuration)
- **Route name input**
- **GPX track upload** (optional, multiple files supported)
  - Drag to reorder segments
  - Automatic simplification (Douglas-Peucker)
  - Combined into single track
- **Checkpoint management**
  - Start/Finish auto-positioned at track endpoints (or manual if no GPX)
  - Click map to add intermediate checkpoints
  - Drag checkpoints on map to reposition
  - Snaps to track if GPX present
  - Set cutoff times (absolute or hours-from-start)
  - Drag table rows to reorder
- **Map visualization**
  - 2-column layout on desktop (form left, sticky map right)
  - Single column on mobile
  - Shows track and all checkpoints
- **Save & Share**
  - Generates compressed URL with route data
  - Can share URL with other riders

#### 2. Tracking Mode (During Event)
- **Auto-populate start time** from event start cutoff (no manual check-in)
- **Sequential check-ins** - must complete previous checkpoint first
- **Pace calculations** (based on original `tcrpace.html` logic):
  - Current pace (actual pace between checkpoints)
  - Required pace (pace needed to meet cutoff)
  - Time ahead/behind schedule
  - ETA for upcoming checkpoints
- **Summary cards** - Overall pace, distance covered, finish ETA
- **Next cutoff display** - Prominent card showing next deadline
- **GPS location** - "Where Am I?" button
  - Snaps to track if within 50km
  - Shows distance to next checkpoint
  - Draws dashed line to nearest point on track
  - If >50km away: "Likely not yet racing"
- **Edit arrival times** - Modal with datetime-local picker
- **Map with recenter control**
- **Collapsible map on mobile** - Saves space for critical data

### Special Considerations

- **Start checkpoint**: Auto-populated from cutoff time, represents event start
- **Finish checkpoint**: Auto-positioned at track end
- **Ferry/transport segments**: Not specially handled yet (edge case)
- **Unknown routes**: Events like "The Unknown Race" where only start/finish known
  - Can work without GPX
  - Manual checkpoint placement

## Technical Implementation

### File Structure
```
pacer/
├── src/
│   ├── main.js              # UI rendering, event handlers (needs refactoring)
│   ├── state.js             # Reactive store, state helpers (✓ good)
│   ├── geo.js               # GPX parsing, turf operations (✓ good)
│   ├── storage.js           # URL compression, localStorage (✓ good)
│   ├── pace.js              # Pace calculations (✓ good)
│   ├── map.js               # Leaflet integration (✓ good)
│   ├── helpers.js           # NEW: Shared utilities (✓ good)
│   ├── checkpoint-ops.js    # NEW: Checkpoint logic (✓ good)
│   └── style.css            # Nested CSS, responsive
├── index.html
├── package.json
└── tcrpace.html             # Original reference implementation (keep)
```

### Key Technologies
- **Vite** - Build tool (requires Node 20.19+ or 22.12+)
- **Turf.js** - Geospatial operations
  - `@turf/helpers`, `@turf/simplify`, `@turf/nearest-point-on-line`
  - `@turf/length`, `@turf/line-slice`, `@turf/distance`
  - `@turf/along`, `@turf/bbox`
- **Leaflet** - Interactive maps
- **Native APIs** - CompressionStream, localStorage, Geolocation

### Data Model

```javascript
// Stored in URL (compressed)
const route = {
  name: "TCR 2025",
  created: "2025-01-15T10:00:00Z",  // Also used as route ID
  track: [[lng, lat], ...],          // Simplified coordinates
  checkpoints: [
    {
      id: "start",                     // Unique ID (not index!)
      name: "Start",
      km: 0,
      coord: [lng, lat],
      cutoff: "2025-07-27T20:00:00",   // Or cutoffHours: 0
    },
    // ... more checkpoints
  ]
};

// Stored in localStorage (keyed by route.created)
const tracking = {
  routeId: "2025-01-15T10:00:00Z",
  arrivals: {
    "start": "2025-07-27T20:00:00",   // Keyed by checkpoint ID
    "cp_abc123": "2025-07-29T15:30:00",
  }
};
```

## Recent Refactoring (Critical to Understand)

### Problem
`main.js` had grown to 1600+ lines with:
- Functions 150+ lines long
- Duplicate code everywhere (date formatting 6+ times, inline messages 8+ times)
- Mixed concerns (UI + logic + data)
- Hard to maintain and test

### Solution
Created two new modules:

#### `helpers.js` - Shared utilities
- `showInlineMessage()` - Replaced all alert() calls and duplicated message rendering
- `formatDateTime()`, `formatDateTimeLocal()`, `formatCutoffTime()` - Date formatting
- `getStartInfo()` - Extract start checkpoint and time
- `canSaveRoute()`, `findMissingCutoffs()` - Validation
- `createModal()` - Modal creation
- `calculateTimeRemaining()`, `formatTimeRemaining()` - Time calculations

#### `checkpoint-ops.js` - Checkpoint business logic
- `calculateCheckpointMetrics()` - All metrics for a table row
- `canCheckIn()` - Sequential validation
- `validateSequentialCheckIn()` - Check-in order validation
- `validateClearCheckpoint()` - Clear validation
- `getCutoffTimeForCheckpoint()` - Cutoff time resolution

**Status:** Modules created but NOT YET fully integrated into `main.js`. Next developer needs to:
1. Import these modules in `main.js`
2. Replace inline logic with function calls
3. Delete duplicated code

## What Works Well

### ✅ Solid Foundation
- **State management** (`state.js`) - Clean reactive store with subscriptions
- **Geospatial ops** (`geo.js`) - Uses turf.js properly, no reinventing the wheel
- **Storage** (`storage.js`) - URL compression works, localStorage keying is correct
- **Pace calculations** (`pace.js`) - Ported from working `tcrpace.html`, accurate
- **Map integration** (`map.js`) - Leaflet abstraction is clean

### ✅ Good UX Decisions
- **2-column setup layout** - Form and map side-by-side on desktop
- **Information density** - Removed visual noise, compact tables, abbreviated headers
- **Inline validation** - No alert() popups, contextual error messages
- **Sequential check-ins** - Prevents illogical progression
- **Auto-populate start time** - One less thing to remember
- **Next cutoff card** - Most important info prominently displayed
- **GPS snapping** - >50km rule prevents confusion before event starts

### ✅ Mobile Optimization
- Horizontal scrolling tables
- Collapsible map
- Abbreviated column headers
- Touch-friendly controls
- Compact spacing

## Known Issues & Limitations

### 🐛 Bugs to Fix
1. **Map initialization race condition** - Sometimes map doesn't render until window resize
2. **Segment reordering doesn't update finish checkpoint immediately** - Need to force recalculation
3. **Dark mode support incomplete** - Some elements don't adapt properly

### ⚠️ Missing Features
1. **No elevation profile** - Could extract from GPX `<ele>` tags
2. **No multi-day events** - No concept of "stages" or rest days
3. **No ferry/transport handling** - Special segments where pace doesn't apply
4. **No offline mode** - Could add service worker
5. **No data export** - Can't export tracking data as CSV/JSON
6. **No "Add to Home Screen" prompt** - Would improve mobile experience
7. **No current location tracking** - Only one-shot "Where Am I?" button
8. **No route reversal** - If riding opposite direction

### 🎯 UX Issues to Improve
1. **Start/Finish placement without GPX** - Just added, needs testing
2. **Cutoff time input** - No timezone handling, assumes local time
3. **Table too wide on mobile** - Even with horizontal scroll, hard to use
4. **No undo/redo** - Easy to accidentally delete checkpoints
5. **No route templates** - Can't save common routes
6. **No guidance for first-time users** - No onboarding or help text

### 🏗️ Technical Debt
1. **`main.js` still too large** - 1400+ lines, needs further breakdown
2. **Refactored modules not integrated** - helpers.js and checkpoint-ops.js exist but not used
3. **No unit tests** - Should test pace calculations, validations, etc.
4. **No TypeScript** - JSDoc types help but not enforced
5. **No build optimization** - Could tree-shake unused turf modules
6. **No error boundaries** - App crashes if something fails
7. **localStorage not versioned** - Breaking changes will lose user data

## Critical Mistakes to Avoid

### ❌ Don't Use Array Indices for Checkpoints
Early versions used array indices, which broke when checkpoints were reordered or deleted. **Always use checkpoint IDs**.

### ❌ Don't Mutate State Directly
Use the store's `update()` or `updateNested()` methods. Direct mutation breaks reactivity.

### ❌ Don't Use `alert()` or `prompt()`
Use inline messages and modals. Better UX, especially on mobile.

### ❌ Don't Auto-Create Checkpoints in Random Locations
Originally created Start/Finish at Berlin coordinates. Confusing and unnecessary.

### ❌ Don't Calculate Pace for Start Checkpoint
Shows "n/a" - there's no previous checkpoint to measure pace from.

### ❌ Don't Allow Checking In Out of Order
Must enforce sequential progression. Original allowed skipping ahead.

### ❌ Don't Forget to Update Start/Finish When Track Changes
When segments are added/removed/reordered, must recalculate finish position.

### ❌ Don't Make Tables Non-Scrollable on Mobile
Allow horizontal scroll, don't try to cram everything into viewport width.

### ❌ Don't Use Manual Implementations When Turf.js Provides It
Use `@turf/along` instead of walking segments, `@turf/bbox` instead of min/max loops.

### ❌ Don't Block the Main Thread
GPX simplification can be slow for large files. Consider web workers if needed.

## Next Steps (Prioritized)

### High Priority
1. **Integrate refactored modules** - Replace duplicated code in main.js
2. **Test start/finish placement** - Just changed, needs validation
3. **Fix map initialization** - Investigate race condition
4. **Add error boundaries** - Catch and display errors gracefully
5. **Test with real GPX files** - Especially multi-segment routes

### Medium Priority
6. **Add unit tests** - At least for pace calculations and validations
7. **Improve mobile table UX** - Consider card view for checkpoints
8. **Add data export** - CSV download of tracking data
9. **Add route templates** - Save/load common routes from localStorage
10. **Better timezone handling** - Make it explicit, not assumed

### Low Priority
11. **Add elevation profile** - Nice to have, not critical
12. **Service worker** - Offline support
13. **Add to Home Screen** - PWA features
14. **TypeScript migration** - Better type safety
15. **Continuous location tracking** - Battery concerns

## Development Setup

```bash
# Install dependencies (requires Node 20.19+ or 22.12+)
npm install

# Run dev server
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

**Important:** If you get Node version errors, the user's setup had issues with mismatched Node/npm versions. Use the npm binary at `/Users/erik/.nvm/versions/node/v24.12.0/bin/npm` if needed.

## Testing Checklist

When making changes, test:
- [ ] Upload single GPX file
- [ ] Upload multiple GPX files, reorder them
- [ ] Delete a segment
- [ ] Add checkpoints via map click
- [ ] Drag checkpoints on map
- [ ] Reorder checkpoints in table
- [ ] Edit cutoff times
- [ ] Save route and reload from URL
- [ ] Check in sequentially
- [ ] Try to skip a checkpoint (should fail)
- [ ] Edit arrival time
- [ ] Use "Where Am I?" with mock GPS
- [ ] Test on mobile viewport
- [ ] Test dark mode
- [ ] Test without GPX (manual start/finish placement)

## Resources

- **Original reference:** `tcrpace.html` - Keep this, it's the working implementation
- **Turf.js docs:** https://turfjs.org/
- **Leaflet docs:** https://leafletjs.com/
- **CompressionStream:** https://developer.mozilla.org/en-US/docs/Web/API/CompressionStream

## Questions for Product Owner

1. **Ferry segments** - How should these be handled? Special checkpoint type?
2. **Timezone handling** - Should cutoffs be UTC or local? Display both?
3. **Multi-day events** - Need rest day support? Stage races?
4. **Data persistence** - Cloud sync? Account system? Or stay local-only?
5. **Sharing tracking data** - Should riders be able to share their progress publicly?
6. **Live tracking** - Continuous GPS vs. manual check-ins?

## Final Notes

This is a focused, well-scoped project that does one thing well. Resist feature creep. The core value is:
1. Easy route setup
2. Reliable tracking during event
3. Accurate pace calculations
4. No server required

Everything else is secondary. When in doubt, refer to the original `tcrpace.html` - it worked, and riders used it successfully.

Good luck! 🚴

