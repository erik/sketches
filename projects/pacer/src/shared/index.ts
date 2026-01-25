import { type LineString, type Position } from "geojson";

/**
 * Branded type for distance in meters.
 * Prevents accidental mixing of units (meters, kilometers, miles, etc).
 */
export type Meters = number & { readonly __brand: "Meters" };

/**
 * Create a Meters value from a number.
 * Use this to explicitly mark a number as representing meters.
 */
export const Meters = (value: number): Meters => value as Meters;

/**
 * Convert Meters to kilometers for display.
 */
export const metersToKm = (meters: Meters): number => meters / 1000;

/**
 * Convert kilometers to Meters.
 */
export const kmToMeters = (km: number): Meters => Meters(km * 1000);

export type EventConfig = {
  name: string;
  notes?: string;
  startTime: Temporal.Instant;
  endTime: Temporal.Instant;
  routeLength: Meters;
  segments: Segment[];
  markers: RouteMarker[];
};

export type Segment = {
  id: string;
  title?: string;
  fileName: string;
  segmentLength: Meters;
  geometry: LineString;
};

export type RouteMarker = {
  id: string;
  kind: "start" | "finish" | "marker" | "control";
  name?: string;
  note?: string;
  icon?: string;
  segmentId?: string;
  routeDistance?: Meters;
  goalTime?: Temporal.Instant;
  cutoffTime?: Temporal.Instant;
  coordinate: Position;
};

export type ControlPointKind = "start" | "finish" | "cp";
export type Coord = { lng: number; lat: number; ele?: number };
export type OldControlPoint = {
  id?: string;
  kind: ControlPointKind;
  name?: string;
  note?: string;
  anchorSegmentId?: string;
  closesAt?: Date;
  coord: Coord;
};
