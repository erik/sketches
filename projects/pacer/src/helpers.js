/**
 * Shared helper functions for UI rendering and data manipulation
 */

import { sortCheckpointsByDistance } from "./geo.js";
import { getCutoffTime } from "./state.js";

/**
 * Show an inline message in a container.
 * @param {HTMLElement} container
 * @param {string} message
 * @param {string} type - "error", "success", or "info"
 * @param {boolean} autoDismiss
 * @returns {HTMLElement|null}
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
 * Format a date as a human-readable string.
 * @param {Date|null} date
 * @returns {string}
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
 * Format a cutoff time for display, handling both absolute and relative times.
 * @param {Object} checkpoint
 * @param {Object} state
 * @returns {string}
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

/**
 * Format a datetime for use in datetime-local input.
 * @param {Date} date
 * @returns {string}
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
 * Scroll an element into view smoothly.
 * @param {string} selector - CSS selector
 */
export function scrollToElement(selector) {
  const element = document.querySelector(selector);
  if (element) {
    element.scrollIntoView({ behavior: "smooth", block: "center" });
  }
}

/**
 * Check if a route can be saved (has name and all CPs have cutoffs).
 * @param {Object} state
 * @returns {boolean}
 */
export function canSaveRoute(state) {
  if (!state.route.name) return false;
  if (state.route.checkpoints.length < 2) return false;

  return state.route.checkpoints.every((cp) => {
    return cp.cutoff || cp.cutoffHours != null;
  });
}

/**
 * Find checkpoints missing cutoff times.
 * @param {Array} checkpoints
 * @returns {Array}
 */
export function findMissingCutoffs(checkpoints) {
  return checkpoints.filter((cp) => !cp.cutoff && cp.cutoffHours == null);
}

/**
 * Create a modal element.
 * @param {string} content - HTML content
 * @param {Function} onClose - Callback when modal closes
 * @returns {HTMLElement}
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
 * Calculate time remaining until a date.
 * @param {Date} targetDate
 * @returns {{days: number, hours: number, minutes: number, isPast: boolean}}
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
 * Format time remaining as a string.
 * @param {{days: number, hours: number, minutes: number}} time
 * @returns {string}
 */
export function formatTimeRemaining({ days, hours, minutes }) {
  const parts = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0 || days > 0) parts.push(`${hours}h`);
  parts.push(`${minutes}m`);
  return parts.join(" ");
}
