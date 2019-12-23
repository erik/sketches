"use strict";

import xml2js from 'xml2js';


// Routes look like this:
//
// All measurements are in meters.
// { name: String
// , totalDistance: Double
// , wayPoints: [
//   { kind: String
//   . name: String
//   . description: String
//   , latitude: Double
//   , longitude: Double
//   }]
// , routePoints: [
//   { latitude: Double
//   , longitude: Double
//   , altitude: Double
//   , cumulativeDistance: Double
//   }]
// }


const readFileContents = (file, isBinary) => {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = (e) => {
      const result = e.target.result;
      return resolve(result);
    };

    if (isBinary) {
      reader.readAsArrayBuffer(file);
    } else {
      reader.readAsText(file);
    }
  });
};

// Get deeply nested key structure, safely.
const get = (root, k) => {
  let n = root;
  for (const part of k.split('.')) {
    n = n[part];
    if (typeof n === 'undefined') break;
  }
  return n;
};

function extractTCXRoute(tcx) {
  if (!tcx.TrainingCenterDatabase) {
    throw new Error('Unexpected formatting for TCX file');
  }
  tcx = tcx.TrainingCenterDatabase;

  if (!tcx.Courses || tcx.Courses.length === 0) {
    throw new Error('No courses defined, is this an activity instead of a route?');
  }

  const course = get(tcx, 'Courses.0.Course.0');

  const route = {
    name: get(course, 'Name.0') || 'Untitled Route',
    totalDistance: 0.0,
    routePoints: [],
    wayPoints: []
  };

  for (let pt of get(course, 'Track.0.Trackpoint')) {
    route.totalDistance = +get(pt, 'DistanceMeters.0');
    route.routePoints.push({
      altitude: +get(pt, 'AltitudeMeters.0') || 0.0,
      latitude: +get(pt, 'Position.0.LatitudeDegrees.0'),
      longitude: +get(pt, 'Position.0.LongitudeDegrees.0'),
      cumulativeDistance: route.totalDistance
    });
  }

  for (let pt of course.CoursePoint) {
    route.wayPoints.push({
      name: get(pt, 'Name.0') || 'Untitled Point',
      description: get(pt, 'Notes.0') || '',
      latitude: +get(pt, 'Position.0.LatitudeDegrees.0'),
      longitude: +get(pt, 'Position.0.LongitudeDegrees.0'),
    });
  }

  return route;
}

// https://stackoverflow.com/a/365853
// TODO: This probably isn't incredible precise, and seems to differ
// from the TCX calculation for the same route by ~1km over 800km
// total. Need to swap this out with a better one at some point.
function distanceBetween(pt1, pt2) {
  const deg2rad = (d) => d * Math.PI / 180;

  const earthRadius = 6371000;

  let dLat = deg2rad(pt2.latitude - pt1.latitude);
  let dLon = deg2rad(pt2.longitude - pt1.longitude);

  const lat1 = deg2rad(pt1.latitude);
  const lat2 = deg2rad(pt2.latitude);

  const a = Math.sin(dLat/2) * Math.sin(dLat/2) +
          Math.sin(dLon/2) * Math.sin(dLon/2) * Math.cos(lat1) * Math.cos(lat2);
  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
  return earthRadius * c;
}

function extractGPXRoute(gpx) {
  if (!gpx.gpx || !gpx.gpx.trk || gpx.gpx.trk.length === 0) {
    throw new Error('Unexpected formatting for GPX file!');
  }

  gpx = gpx.gpx;

  const route = {
    name: get(gpx, 'metadata.0.name.0') || 'Untitled Route',
    totalDistance: 0.0,
    routePoints: [],
    wayPoints: []
  };

  let lastPt = null;
  for (let pt of get(gpx, 'trk.0.trkseg.0.trkpt')) {
    const coord = {
      latitude: +get(pt, '$.lat'),
      longitude: +get(pt, '$.lon'),
    };

    route.totalDistance += lastPt === null ? 0 : distanceBetween(lastPt, coord);

    route.routePoints.push({
      ...coord,
      altitude: +get(pt, 'ele.0') || 0.0,
      cumulativeDistance: route.totalDistance
    });

    lastPt = coord;
  }

  for (let pt of gpx.wpt) {
    route.wayPoints.push({
      name: get(pt, 'name.0') || 'Untitled Point',
      description: get(pt, 'desc.0') || '',
      latitude: +get(pt, '$.lat'),
      longitude: +get(pt, '$.lon'),
    });
  }

  return route;
}

async function parseXML(file) {
  const contents = await readFileContents(file, false);
  const xmlParser = new xml2js.Parser();

  return new Promise((resolve, reject) => {
    xmlParser.parseString(contents, (err, result) => {
      if (err) {
        reject(err);
      } else {
        resolve(result);
      }
    });
  });
}

export async function parseFile(file) {
  const extension = file.name.split('.').pop();
  switch ((extension || '').toUpperCase()) {
  case 'TCX': {
    const xml = await parseXML(file);
    return extractTCXRoute(xml);
  }

  case 'GPX': {
    const xml = await parseXML(file);
    return extractGPXRoute(xml);
  }

  default:
    throw new Error(`Unsupported file type: ${extension}`);
  }
}


export default { parseFile };
