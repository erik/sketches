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

function extractTCXRoute(tcx) {
  if (!tcx.TrainingCenterDatabase) {
    throw new Error('Unexpected formatting for TCX file');
  }
  tcx = tcx.TrainingCenterDatabase;

  if (!tcx.Courses || tcx.Courses.length === 0) {
    throw new Error('No courses defined, is this an activity instead of a route?');
  }

  // Get deeply nested key structure, safely.
  const get = (root, k) => {
    let n = root;
    for (const part of k.split('.')) {
      n = n[part];
      if (typeof n === 'undefined') break;
    }
    return n;
  };

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

async function parseTCX(file) {
  const contents = await readFileContents(file, false);
  const xmlParser = new xml2js.Parser();

  return new Promise((resolve, reject) => {
    xmlParser.parseString(contents, (err, result) => {
      if (err) {
        reject(err);
      } else {
        resolve(extractTCXRoute(result));
      }
    });
  });
}

export async function parseFile(file) {
  const extension = file.name.split('.').pop();
  switch ((extension || '').toUpperCase()) {
  case 'TCX':
    return await parseTCX(file);

  default:
    throw new Error(`Unsupported file type: ${extension}`);
  }
}


export default { parseFile };
