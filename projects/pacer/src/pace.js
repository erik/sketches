/**
 * Pace calculation utilities for tracking mode.
 * Based on tcrpace.html logic.
 */

/**
 * Get time difference in hours between two dates.
 * @param {Date} laterTime
 * @param {Date} earlierTime
 * @returns {number} Hours difference
 */
export function getTimeDifferenceHours(laterTime, earlierTime) {
  if (!laterTime || !earlierTime) return 0;
  return (laterTime.getTime() - earlierTime.getTime()) / (1000 * 60 * 60);
}

/**
 * Format time difference as a human-readable string.
 * @param {Date} laterTime
 * @param {Date} earlierTime
 * @returns {string}
 */
export function formatTimeDifference(laterTime, earlierTime) {
  if (!laterTime || !earlierTime) return "-";

  let diff = getTimeDifferenceHours(laterTime, earlierTime);
  const isNegative = diff < 0;
  diff = Math.abs(diff);

  const days = Math.floor(diff / 24);
  const hours = Math.floor(diff % 24);
  const minutes = Math.floor((diff * 60) % 60);

  const parts = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0 || days > 0) parts.push(`${hours}h`);
  parts.push(`${minutes}m`);

  return (isNegative ? "-" : "") + parts.join(" ");
}

/**
 * Calculate actual pace between two checkpoints.
 * @param {Object} tracking - Tracking data with arrivals
 * @param {Object} fromCp - Starting checkpoint
 * @param {Object} toCp - Ending checkpoint
 * @returns {number|null} Pace in km/h
 */
export function calculateCurrentPace(tracking, fromCp, toCp) {
  const fromTime = tracking.arrivals[fromCp.id];
  const toTime = tracking.arrivals[toCp.id];

  if (!fromTime || !toTime) return null;

  const timeDiff = getTimeDifferenceHours(
    new Date(toTime),
    new Date(fromTime),
  );
  const distance = toCp.km - fromCp.km;

  if (timeDiff <= 0) return null;

  return distance / timeDiff;
}

/**
 * Calculate required pace to reach a checkpoint by its cutoff.
 * @param {Object} tracking - Tracking data with arrivals
 * @param {Object} fromCp - Starting checkpoint
 * @param {Object} toCp - Target checkpoint
 * @param {Date} startTime - Race start time
 * @returns {number|null} Required pace in km/h
 */
export function calculateRequiredPace(tracking, fromCp, toCp, startTime) {
  // Use actual arrival time if available, otherwise use cutoff
  const fromTime = tracking.arrivals[fromCp.id]
    ? new Date(tracking.arrivals[fromCp.id])
    : getCutoffTime(fromCp, startTime);

  const toTime = getCutoffTime(toCp, startTime);

  if (!fromTime || !toTime) return null;

  const timeDiff = getTimeDifferenceHours(toTime, fromTime);
  const distance = toCp.km - fromCp.km;

  if (timeDiff <= 0) return null;

  return distance / timeDiff;
}

/**
 * Calculate estimated arrival at a checkpoint based on overall pace.
 * @param {Object} tracking - Tracking data with arrivals
 * @param {Array} checkpoints - All checkpoints (sorted)
 * @param {Object} targetCp - Target checkpoint
 * @returns {Date|null}
 */
export function calculateEstimatedArrival(tracking, checkpoints, targetCp) {
  // Find last reached checkpoint
  const lastReached = findLastReachedCheckpoint(checkpoints, tracking);
  if (!lastReached) return null;

  const lastReachedTime = new Date(tracking.arrivals[lastReached.id]);
  const startCp = checkpoints[0];
  const startTime = tracking.arrivals[startCp.id]
    ? new Date(tracking.arrivals[startCp.id])
    : null;

  if (!startTime) return null;

  // Calculate overall pace from start to last reached
  const totalTime = getTimeDifferenceHours(lastReachedTime, startTime);
  const totalDistance = lastReached.km - startCp.km;

  if (totalTime <= 0 || totalDistance <= 0) return null;

  const overallPace = totalDistance / totalTime;

  // Calculate distance remaining to target
  const distanceToTarget = targetCp.km - lastReached.km;

  // Time needed in hours
  const timeNeeded = distanceToTarget / overallPace;

  return new Date(lastReachedTime.getTime() + timeNeeded * 60 * 60 * 1000);
}

/**
 * Calculate time ahead or behind schedule at a checkpoint.
 * @param {Object} tracking - Tracking data with arrivals
 * @param {Object} checkpoint - The checkpoint
 * @param {Date} startTime - Race start time
 * @returns {number|null} Hours ahead (positive) or behind (negative)
 */
export function calculateTimeAheadBehind(tracking, checkpoint, startTime) {
  const arrivalTime = tracking.arrivals[checkpoint.id];
  if (!arrivalTime) return null;

  const cutoffTime = getCutoffTime(checkpoint, startTime);
  if (!cutoffTime) return null;

  return getTimeDifferenceHours(cutoffTime, new Date(arrivalTime));
}

/**
 * Get cutoff time for a checkpoint.
 * @param {Object} checkpoint
 * @param {Date} startTime
 * @returns {Date|null}
 */
function getCutoffTime(checkpoint, startTime) {
  if (checkpoint.cutoff) {
    return new Date(checkpoint.cutoff);
  }
  if (checkpoint.cutoffHours != null && startTime) {
    return new Date(startTime.getTime() + checkpoint.cutoffHours * 3600000);
  }
  return null;
}

/**
 * Find the last checkpoint that has been reached.
 * @param {Array} checkpoints - Sorted checkpoints
 * @param {Object} tracking - Tracking data
 * @returns {Object|null}
 */
function findLastReachedCheckpoint(checkpoints, tracking) {
  for (let i = checkpoints.length - 1; i >= 0; i--) {
    if (tracking.arrivals[checkpoints[i].id]) {
      return checkpoints[i];
    }
  }
  return null;
}

/**
 * Calculate overall statistics for summary display.
 * @param {Object} tracking - Tracking data
 * @param {Array} checkpoints - All checkpoints (sorted)
 * @param {Date} startTime - Race start time
 * @returns {Object} Summary stats
 */
export function calculateSummaryStats(tracking, checkpoints, startTime) {
  const stats = {
    overallPace: 0,
    distanceCovered: 0,
    timeAheadBehind: 0,
    timeAheadBehindStr: "-",
    estimatedFinish: null,
  };

  const startCp = checkpoints[0];
  if (!tracking.arrivals[startCp.id]) return stats;

  const lastReached = findLastReachedCheckpoint(checkpoints, tracking);
  if (!lastReached || lastReached.id === startCp.id) return stats;

  // Calculate overall pace
  const totalTime = getTimeDifferenceHours(
    new Date(tracking.arrivals[lastReached.id]),
    new Date(tracking.arrivals[startCp.id]),
  );
  const totalDistance = lastReached.km - startCp.km;

  if (totalTime > 0 && totalDistance > 0) {
    stats.overallPace = totalDistance / totalTime;
    stats.distanceCovered = lastReached.km;
  }

  // Time ahead/behind at last checkpoint
  const timeAheadBehind = calculateTimeAheadBehind(
    tracking,
    lastReached,
    startTime,
  );
  if (timeAheadBehind !== null) {
    stats.timeAheadBehind = timeAheadBehind;
    stats.timeAheadBehindStr = formatTimeDifference(
      getCutoffTime(lastReached, startTime),
      new Date(tracking.arrivals[lastReached.id]),
    );
  }

  // Estimated finish
  const finishCp = checkpoints[checkpoints.length - 1];
  if (lastReached.id !== finishCp.id) {
    stats.estimatedFinish = calculateEstimatedArrival(
      tracking,
      checkpoints,
      finishCp,
    );
  } else {
    stats.estimatedFinish = new Date(tracking.arrivals[finishCp.id]);
  }

  return stats;
}

/**
 * Get the current checkpoint index (last one reached).
 * @param {Array} checkpoints - Sorted checkpoints
 * @param {Object} tracking - Tracking data
 * @returns {number}
 */
export function getCurrentCheckpointIndex(checkpoints, tracking) {
  for (let i = checkpoints.length - 1; i >= 0; i--) {
    if (tracking.arrivals[checkpoints[i].id]) {
      return i;
    }
  }
  return 0;
}
