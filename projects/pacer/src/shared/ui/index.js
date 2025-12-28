/**
 * UI Module - Pure UI-related helpers
 * No dependencies on geo, state, or business logic
 * Handles formatting, messages, and UI interactions
 */

import { sortCheckpointsByDistance } from "../geo/index.js";
import { getCutoffTime } from "../state.js";

/**
 * Show inline message in container
 * @param {HTMLElement} container - Container element
 * @param {string} message - Message text
 * @param {string} type - "error", "success", or "info"
 * @param {boolean} autoDismiss - Auto-dismiss after timeout
 * @returns {HTMLElement|null} Message element
 */
export function showInlineMessage(
  container,
  message,
  type = "error",
  autoDismiss = true,
) {
  if (!container) return null;

  // Remove existing message
  const existing = container.querySelector(".inline-message");
  if (existing) existing.remove();

  const msgEl = document.createElement("p");
  msgEl.className = `inline-message ${type}`;
  msgEl.style.fontWeight = "500";
  msgEl.style.marginTop = "0.5rem";
  msgEl.style.marginBottom = "0.5rem";
  msgEl.style.padding = "0.5rem";
  msgEl.style.borderRadius = "4px";
  msgEl.textContent = message;

  const styles = {
    error: { color: "#dc2626", backgroundColor: "#fef2f2" },
    success: { color: "#059669", backgroundColor: "#f0fdf4" },
    info: { color: "#6b7280", backgroundColor: "#f9fafb" },
  };

  const style = styles[type];
  if (style) {
    msgEl.style.color = style.color;
    msgEl.style.backgroundColor = style.backgroundColor;
  }

  container.appendChild(msgEl);

  if (autoDismiss) {
    setTimeout(() => msgEl.remove(), 5000);
  }

  return msgEl;
}

/**
 * Format date for display
 * @param {Date|null} date - Date to format
 * @returns {string} Formatted date
 */
export function formatDateTime(date) {
  if (!date || isNaN(date.getTime())) return "-";

  const options = {
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  };
  return date.toLocaleString("en-US", options);
}

/**
 * Format cutoff time with context
 * @param {Object} checkpoint - Checkpoint object
 * @param {Object} state - Application state
 * @returns {string} Formatted cutoff time
 */
export function formatCutoffTime(checkpoint, state) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const startCp = sorted[0];
  const startTime = state.tracking.arrivals[startCp?.id]
    ? new Date(state.tracking.arrivals[startCp.id])
    : null;

  if (checkpoint.cutoff) {
    return formatDateTime(new Date(checkpoint.cutoff));
  }

  if (checkpoint.cutoffHours != null) {
    if (startTime) {
      const cutoffTime = new Date(
        startTime.getTime() + checkpoint.cutoffHours * 3600000,
      );
      return `${formatDateTime(cutoffTime)} (${checkpoint.cutoffHours}h from start)`;
    }
    return `${checkpoint.cutoffHours}h from start`;
  }

  return "-";
}

/**
 * Format date for datetime-local input
 * @param {Date} date - Date to format
 * @returns {string} Formatted datetime
 */
export function formatDateTimeLocal(date) {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hours = String(date.getHours()).padStart(2, "0");
  const minutes = String(date.getMinutes()).padStart(2, "0");
  return `${year}-${month}-${day}T${hours}:${minutes}`;
}

/**
 * Scroll element into view
 * @param {string} selector - CSS selector
 */
export function scrollToElement(selector) {
  const element = document.querySelector(selector);
  if (element) {
    element.scrollIntoView({ behavior: "smooth", block: "center" });
  }
}

/**
 * Create modal dialog
 * @param {string} content - HTML content
 * @param {Function} onClose - Close callback
 * @returns {HTMLElement} Modal element
 */
export function createModal(content, onClose) {
  const modal = document.createElement("div");
  modal.className = "edit-time-modal";
  modal.innerHTML = `<div class="edit-time-modal-content">${content}</div>`;

  // Click outside to close
  modal.addEventListener("click", (e) => {
    if (e.target === modal) {
      modal.remove();
      if (onClose) onClose();
    }
  });

  // Escape key to close
  const escapeHandler = (e) => {
    if (e.key === "Escape") {
      modal.remove();
      document.removeEventListener("keydown", escapeHandler);
      if (onClose) onClose();
    }
  };
  document.addEventListener("keydown", escapeHandler);

  return modal;
}

/**
 * Calculate time remaining until target
 * @param {Date} targetDate - Target date
 * @returns {Object} Time remaining components
 */
export function calculateTimeRemaining(targetDate) {
  const now = new Date();
  const diff = targetDate.getTime() - now.getTime();
  const isPast = diff < 0;
  const absDiff = Math.abs(diff);

  const days = Math.floor(absDiff / (1000 * 60 * 60 * 24));
  const hours = Math.floor(
    (absDiff % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60),
  );
  const minutes = Math.floor((absDiff % (1000 * 60 * 60)) / (1000 * 60));

  return { days, hours, minutes, isPast };
}

/**
 * Format time remaining as string
 * @param {Object} time - Time components
 * @returns {string} Formatted time
 */
export function formatTimeRemaining({ days, hours, minutes }) {
  const parts = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0 || days > 0) parts.push(`${hours}h`);
  parts.push(`${minutes}m`);
  return parts.join(" ");
}

/**

/**
 * Calculate current pace between checkpoints
 * @param {Object} prevCp - Previous checkpoint
 * @param {Object} currCp - Current checkpoint
 * @param {Object} tracking - Tracking data
 * @returns {number|null} Pace in km/h
 */
export function calculateCurrentPace(prevCp, currCp, tracking) {
  const prevArrival = new Date(tracking.arrivals[prevCp.id]);
  const currArrival = new Date(tracking.arrivals[currCp.id]);
  const timeDiffHours = (currArrival - prevArrival) / (1000 * 60 * 60);
  const distance = currCp.km - prevCp.km;

  if (timeDiffHours > 0 && distance > 0) {
    return distance / timeDiffHours;
  }
  return null;
}

/**
 * Calculate required pace to meet cutoff
 * @param {Object} checkpoint - Target checkpoint
 * @param {Object} prevCp - Previous checkpoint
 * @param {Date} startTime - Event start time
 * @returns {number|null} Required pace in km/h
 */
export function calculateRequiredPace(checkpoint, prevCp, startTime) {
  const cutoffTime = getCutoffTime(checkpoint, startTime);
  if (!cutoffTime) return null;

  const now = new Date();
  const timeRemainingHours = (cutoffTime - now) / (1000 * 60 * 60);
  const distanceRemaining = checkpoint.km - prevCp.km;

  if (timeRemainingHours > 0 && distanceRemaining > 0) {
    return distanceRemaining / timeRemainingHours;
  }
  return null;
}

/**
 * Calculate estimated arrival time
 * @param {Object} checkpoint - Target checkpoint
 * @param {Object} prevCp - Previous checkpoint
 * @param {Object} tracking - Tracking data
 * @returns {Date|null} Estimated arrival time
 */
export function calculateEstimatedArrival(checkpoint, prevCp, tracking) {
  const prevArrival = new Date(tracking.arrivals[prevCp.id]);
  const currentPace = calculateCurrentPace(prevCp, checkpoint, tracking);
  if (!currentPace) return null;

  const distanceRemaining = checkpoint.km - prevCp.km;
  const timeRemainingHours = distanceRemaining / currentPace;
  return new Date(prevArrival.getTime() + timeRemainingHours * 60 * 60 * 1000);
}

/**
 * Calculate time ahead/behind schedule
 * @param {Object} checkpoint - Current checkpoint
 * @param {Object} tracking - Tracking data
 * @param {Date} startTime - Event start time
 * @returns {number|null} Hours ahead (+) or behind (-)
 */
export function calculateTimeAheadBehind(checkpoint, tracking, startTime) {
  const cutoffTime = getCutoffTime(checkpoint, startTime);
  const arrivalTime = new Date(tracking.arrivals[checkpoint.id]);

  if (cutoffTime && arrivalTime) {
    return (arrivalTime - cutoffTime) / (1000 * 60 * 60);
  }
  return null;
}

/**
 * Calculate summary statistics
 * @param {Object} tracking - Tracking data
 * @param {Array} checkpoints - All checkpoints
 * @param {Date} startTime - Event start time
 * @returns {Object} Summary statistics
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

  // Overall pace
  const totalTime = getTimeDifferenceHours(
    new Date(tracking.arrivals[lastReached.id]),
    new Date(tracking.arrivals[startCp.id]),
  );
  const totalDistance = lastReached.km - startCp.km;

  if (totalTime > 0 && totalDistance > 0) {
    stats.overallPace = totalDistance / totalTime;
    stats.distanceCovered = lastReached.km;
  }

  // Time ahead/behind
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
      lastReached,
    );
  }

  return stats;
}

/**
 * Find last reached checkpoint
 * @param {Array} checkpoints - All checkpoints
 * @param {Object} tracking - Tracking data
 * @returns {Object|null} Last reached checkpoint
 */
export function findLastReachedCheckpoint(checkpoints, tracking) {
  const reached = checkpoints.filter((cp) => tracking.arrivals[cp.id]);
  return reached.length > 0 ? reached[reached.length - 1] : null;
}

/**
 * Get current checkpoint index
 * @param {Array} checkpoints - All checkpoints
 * @param {Object} tracking - Tracking data
 * @returns {number} Current checkpoint index
 */
export function getCurrentCheckpointIndex(checkpoints, tracking) {
  for (let i = 0; i < checkpoints.length; i++) {
    if (tracking.arrivals[checkpoints[i].id]) continue;
    return i;
  }
  return checkpoints.length - 1;
}

/**
 * Format time difference
 * @param {Date} target - Target time
 * @param {Date} actual - Actual time
 * @returns {string} Formatted difference
 */
export function formatTimeDifference(target, actual) {
  const diff = actual - target;
  const absDiff = Math.abs(diff);
  const hours = absDiff / (1000 * 60 * 60);
  return `${diff > 0 ? "+" : ""}${hours.toFixed(1)}h`;
}

/**
 * Get time difference in hours
 * @param {Date} date1 - First date
 * @param {Date} date2 - Second date
 * @returns {number} Hours difference
 */
export function getTimeDifferenceHours(date1, date2) {
  return Math.abs(date1 - date2) / (1000 * 60 * 60);
}

/**
 * Check if route can be saved
 * @param {Object} state - Application state
 * @returns {boolean} True if savable
 */
export function canSaveRoute(state) {
  if (!state.route.name) return false;
  if (state.route.checkpoints.length < 2) return false;
  return state.route.checkpoints.every((cp) => {
    return cp.cutoff || cp.cutoffHours != null;
  });
}

/**
 * Find checkpoints missing cutoffs
 * @param {Array} checkpoints - Checkpoints to check
 * @returns {Array} Missing cutoff checkpoints
 */
export function findMissingCutoffs(checkpoints) {
  return checkpoints.filter((cp) => !cp.cutoff && cp.cutoffHours == null);
}

/**
 * Get the start checkpoint and start time from state.
 * @param {Object} state
 * @returns {{startCp: Object|null, startTime: Date|null}}
 */
export function getStartInfo(state) {
  const sorted = sortCheckpointsByDistance(state.route.checkpoints);
  const startCp = sorted[0] || null;
  const startTime =
    startCp && state.tracking.arrivals[startCp.id]
      ? new Date(state.tracking.arrivals[startCp.id])
      : startCp?.cutoff
        ? new Date(startCp.cutoff)
        : null;

  return { startCp, startTime };
}
