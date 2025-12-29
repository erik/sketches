/**
 * Shared helper functions for UI rendering and data manipulation
 */

import { sortCheckpointsByDistance } from "./geo.js";

/**
 * Generate a unique ID for checkpoints.
 * @returns {string}
 */
export function generateId() {
  return "cp_" + Math.random().toString(36).substring(2, 10);
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
