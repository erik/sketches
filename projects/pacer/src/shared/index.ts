import { type LineString, type Position } from "geojson";

export type EventConfig = {
  name: string;
  notes?: string;
  startTime: Date;
  endTime: Date;
  routeLength: number;
  segments: Segment[];
  markers: RouteMarker[];
};

export type Segment = {
  id: string;
  title?: string;
  fileName: string;
  segmentLength: number;
  geometry: LineString;
};

export type RouteMarker = {
  id: string;
  kind: "start" | "finish" | "marker" | "control";
  name?: string;
  note?: string;
  icon?: string;
  segmentId?: string;
  routeDistance?: number;
  goalTime?: Date;
  cutoffTime?: Date;
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
