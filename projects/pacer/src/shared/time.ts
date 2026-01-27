const TIME_FORMAT = new Intl.DateTimeFormat(undefined, {
  hour: "2-digit",
  minute: "2-digit",
  hour12: false,
});

const DATE_FORMAT = new Intl.DateTimeFormat(undefined, {
  month: "short",
  day: "numeric",
});

// Example: "14:30 Feb 1"
export function formatDateTimeCompact(
  instant: Temporal.Instant | null,
): string {
  if (instant == null) return "--";

  const zdt = instant.toZonedDateTimeISO(Temporal.Now.timeZoneId());

  const time = TIME_FORMAT.format(zdt.toInstant());
  const date = DATE_FORMAT.format(zdt.toInstant());
  return `${time} ${date}`;
}

// Example: "2d 5h 30m"
export function formatDuration(duration: Temporal.Duration): string {
  const days = Math.floor(duration.total({ unit: "days" }));
  const hours = Math.floor(duration.total({ unit: "hours" }) % 24);
  const minutes = Math.floor(duration.total({ unit: "minutes" }) % 60);

  const parts = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0) parts.push(`${hours}h`);
  if (minutes > 0) parts.push(`${minutes}m`);
  if (parts.length === 0) parts.push("n/a");

  return parts.join(" ");
}

export function formatRelativeTime(time: Temporal.Instant | null): string {
  if (time == null) return "??";

  const now = Temporal.Now.instant();
  const duration = time.since(now);
  return formatDuration(duration.abs());
}

// For <input type="datetime-local">
export function instantToDateTimeLocal(
  instant: Temporal.Instant | undefined | null,
): string {
  if (!instant) return "";

  return instant
    .toZonedDateTimeISO(Temporal.Now.timeZoneId())
    .toPlainDateTime()
    .toString()
    .slice(0, 16);
}

// For <input type="datetime-local">
export function dateTimeLocalToInstant(value: string): Temporal.Instant {
  return Temporal.PlainDateTime.from(value)
    .toZonedDateTime(Temporal.Now.timeZoneId())
    .toInstant();
}
