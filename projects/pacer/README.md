# Pacer - Endurance Cycling Tracker

A lightweight, client-side application for tracking progress on endurance cycling events. No backend required - all route data is stored in the URL, and tracking data is stored locally in your browser.

## Features

- **Setup Mode**: Upload GPX tracks (or work without them), define checkpoints, set cutoffs
- **Interactive Map**: Visualize tracks, position checkpoints, see your location during tracking
- **Flexible Route Setup**: 
  - Upload one or more GPX files (drag to reorder segments)
  - Or skip GPX entirely (for events like "The Unknown Race" with unknown routes)
  - Auto-generated Start/Finish checkpoints
- **Tracking Mode**: Check in at checkpoints, view pace calculations and ETAs, get GPS location
- **URL-based sharing**: Route configuration compressed into shareable URLs
- **Offline-friendly**: Tracking data stored in localStorage
- **No server required**: Pure client-side JavaScript

## Tech Stack

- Vanilla JavaScript (ES modules)
- Vite for development and building
- Turf.js for geospatial operations (GPX parsing, track simplification, distance calculations)
- Leaflet for interactive maps
- Native CompressionStream API for URL compression
- CSS with nested syntax

## Getting Started

### Prerequisites

- Node.js 20.19+ or 22.12+

### Installation

```bash
npm install
```

### Development

```bash
npm run dev
```

### Build

```bash
npm run build
```

## Architecture

### Data Model

**Route Data** (stored in URL hash, compressed):
- Route name and creation timestamp
- Simplified GPX track (array of [lng, lat] coordinates)
- Checkpoints with unique IDs, names, distances, and cutoff times

**Tracking Data** (stored in localStorage, keyed by route ID):
- Arrival times for each checkpoint (keyed by checkpoint ID)

### File Structure

```
pacer/
├── src/
│   ├── main.js       # Main app, UI rendering, event handlers
│   ├── state.js      # Reactive store, state helpers
│   ├── storage.js    # URL compression, localStorage
│   ├── geo.js        # GPX parsing, track simplification, geo ops
│   ├── map.js        # Leaflet map integration and visualization
│   └── style.css     # Styles with nested CSS
├── index.html
└── package.json
```

## Usage

### Setup Mode

1. **Enter a route name**

2. **Upload GPX track(s)** (optional):
   - Upload one or more GPX files
   - Drag to reorder segments if multiple files
   - Tracks are automatically parsed and simplified
   - Start/Finish checkpoints auto-positioned at track endpoints

3. **OR skip GPX** for unknown routes:
   - Start/Finish checkpoints created near Berlin (default map center)
   - Drag markers on map to position
   - Manually enter distances

4. **Add checkpoints**:
   - Click on track in map to add checkpoint (auto-snaps to track)
   - Or use "Add Intermediate Checkpoint" button
   - Drag checkpoints on map to reposition
   - Set cutoff times (absolute datetime or hours from start)

5. **Click "Save & Generate URL"** to create shareable link

### Tracking Mode

1. **Share the URL or bookmark it**
2. **View the map** showing full route and checkpoints
3. **Click "Where Am I?"** to get your GPS location:
   - Shows your position on map
   - Snaps to track if available
   - Displays distance to next checkpoint
4. **Check in** at each checkpoint as you reach it
5. **View statistics**:
   - Overall pace
   - Distance covered
   - Time ahead/behind schedule
   - Estimated finish time

## Design Decisions

### Why URL-based storage?
- Makes routes easily shareable
- No backend or database needed
- Works on any static host (GitHub Pages, Netlify, etc.)
- iOS can store large URL fragments

### Why separate route and tracking data?
- Route is shareable (in URL)
- Tracking is personal (in localStorage)
- Editing checkpoints won't break existing tracking data (using IDs not indices)

### GPX Simplification
- Original tracks can be 10,000+ points
- Douglas-Peucker simplification reduces to ~500-2000 points
- Tolerance of 0.001° (~100m) maintains accuracy for snapping
- Dramatically reduces URL size

### Cutoff Times
Supports two formats:
- **Absolute**: Fixed datetime (e.g., "2025-07-30T09:00:00")
- **Relative**: Hours from start (e.g., 61 hours)
- Relative times are more reusable across event years

### Start/Finish Checkpoints
- **With GPX Track**: Auto-generated at track endpoints, locked to track
- **Without GPX Track**: Draggable on map, manually positioned

### Map Features
- **Setup Mode**:
  - Click on track to add checkpoints
  - Drag checkpoints to reposition (snaps to track if available)
  - Visualize multi-segment routes
- **Tracking Mode**:
  - View complete route
  - Get GPS location ("Where Am I?" button)
  - See position snapped to track
  - Distance calculations to next checkpoint

## Future Enhancements

- [ ] Pace calculations and live ETAs
- [ ] Time ahead/behind schedule display
- [ ] Elevation profile visualization
- [ ] Multiple route support (stage races)
- [ ] Export/import tracking data
- [ ] Add to Home Screen prompt for mobile
- [ ] Offline service worker
- [ ] Dark mode toggle
