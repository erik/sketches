/**
 * Generate a unique ID for checkpoints.
 * @returns {string}
 */
export function generateId() {
  return "cp_" + Math.random().toString(36).substring(2, 10);
}
