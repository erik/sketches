/**
 * Checkpoint-specific operations and calculations
 */

import {
  calculateCurrentPace,
  calculateRequiredPace,
  calculateEstimatedArrival,
  calculateTimeAheadBehind,
  getCurrentCheckpointIndex,
} from "./pace.js";
import { getStartInfo } from "./ui/index.js";

/**
 * Calculate all metrics for a checkpoint row in tracking table.
 * @param {Object} checkpoint
 * @param {number} index
 * @param {Array} sorted - Sorted checkpoints
 * @param {Object} state
 * @returns {Object} Metrics for display
 */
export function calculateCheckpointMetrics(checkpoint, index, sorted, state) {
  const arrival = state.tracking.arrivals[checkpoint.id];
  const hasArrived = !!arrival;
  const prevCp = index > 0 ? sorted[index - 1] : null;
  const { startTime } = getStartInfo(state);
  const currentIndex = getCurrentCheckpointIndex(sorted, state.tracking);

  // Distance from last checkpoint
  const distFromLast = prevCp ? (checkpoint.km - prevCp.km).toFixed(1) : "0.0";

  // Initialize metrics
  let currentPace = null;
  let requiredPace = null;
  let timeAheadBehind = null;
  let estimatedArrival = null;
  let remainingTimeStr = null;

  // Skip pace calculations for start checkpoint
  if (index > 0) {
    if (hasArrived && prevCp) {
      currentPace = calculateCurrentPace(state.tracking, prevCp, checkpoint);
    }

    if (currentIndex < index && startTime) {
      const fromCp = sorted[currentIndex];
      requiredPace = calculateRequiredPace(
        state.tracking,
        fromCp,
        checkpoint,
        startTime,
      );
    }

    if (hasArrived && startTime) {
      timeAheadBehind = calculateTimeAheadBehind(
        state.tracking,
        checkpoint,
        startTime,
      );
    }

    if (!hasArrived && index > currentIndex && startTime) {
      estimatedArrival = calculateEstimatedArrival(
        state.tracking,
        sorted,
        checkpoint,
      );

      // Calculate remaining time to cutoff
      const cutoffTime = getCutoffTimeForCheckpoint(checkpoint, startTime);
      if (cutoffTime) {
        const now = new Date();
        const remaining = cutoffTime.getTime() - now.getTime();
        if (remaining > 0) {
          remainingTimeStr = formatRemainingTime(remaining);
        }
      }
    }
  }

  return {
    hasArrived,
    arrival,
    distFromLast,
    currentPace,
    requiredPace,
    timeAheadBehind,
    estimatedArrival,
    remainingTimeStr,
  };
}

/**
 * Check if a checkpoint can be checked into (sequential validation).
 * @param {Object} checkpoint
 * @param {number} index
 * @param {Array} sorted
 * @param {Object} state
 * @returns {boolean}
 */
export function canCheckIn(checkpoint, index, sorted, state) {
  // Already checked in
  if (state.tracking.arrivals[checkpoint.id]) {
    return false;
  }

  // Start checkpoint can't be manually checked in
  if (checkpoint.id === "start") {
    return false;
  }

  // First checkpoint or all previous are complete
  if (index === 0) return true;

  for (let i = 0; i < index; i++) {
    if (!state.tracking.arrivals[sorted[i].id]) {
      return false;
    }
  }

  return true;
}

/**
 * Find the next uncompleted checkpoint.
 * @param {Array} checkpoints - Sorted checkpoints
 * @param {Object} tracking
 * @returns {Object|null}
 */
export function findNextCheckpoint(checkpoints, tracking) {
  for (const cp of checkpoints) {
    if (!tracking.arrivals[cp.id]) {
      return cp;
    }
  }
  return null;
}

/**
 * Get cutoff time for a checkpoint.
 * @param {Object} checkpoint
 * @param {Date|null} startTime
 * @returns {Date|null}
 */
export function getCutoffTimeForCheckpoint(checkpoint, startTime) {
  if (checkpoint.cutoff) {
    return new Date(checkpoint.cutoff);
  }
  if (checkpoint.cutoffHours != null && startTime) {
    return new Date(startTime.getTime() + checkpoint.cutoffHours * 3600000);
  }
  return null;
}

/**
 * Format remaining time in a human-readable way.
 * @param {number} milliseconds
 * @returns {string}
 */
function formatRemainingTime(milliseconds) {
  const days = Math.floor(milliseconds / (1000 * 60 * 60 * 24));
  const hours = Math.floor(
    (milliseconds % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60),
  );
  const minutes = Math.floor((milliseconds % (1000 * 60 * 60)) / (1000 * 60));

  const parts = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0 || days > 0) parts.push(`${hours}h`);
  parts.push(`${minutes}m`);

  return parts.join(" ");
}

/**
 * Validate that a checkpoint can be cleared.
 * @param {string} checkpointId
 * @param {Array} sorted - Sorted checkpoints
 * @returns {{canClear: boolean, reason: string|null}}
 */
export function validateClearCheckpoint(checkpointId, sorted) {
  const startCp = sorted[0];

  if (checkpointId === startCp.id) {
    return {
      canClear: false,
      reason: "Cannot clear start time. This is set from the event start time.",
    };
  }

  return { canClear: true, reason: null };
}

/**
 * Validate sequential check-in (must check in at previous CPs first).
 * @param {string} checkpointId
 * @param {Array} sorted
 * @param {Object} tracking
 * @returns {{isValid: boolean, missingCheckpoint: Object|null}}
 */
export function validateSequentialCheckIn(checkpointId, sorted, tracking) {
  const cpIndex = sorted.findIndex((cp) => cp.id === checkpointId);

  if (cpIndex <= 0) {
    return { isValid: true, missingCheckpoint: null };
  }

  for (let i = 0; i < cpIndex; i++) {
    if (!tracking.arrivals[sorted[i].id]) {
      return { isValid: false, missingCheckpoint: sorted[i] };
    }
  }

  return { isValid: true, missingCheckpoint: null };
}
