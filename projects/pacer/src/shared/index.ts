import { type LineString } from "geojson";

export type Coord = { lng: number; lat: number };

export type EventConfig = {
  name: string;
  startTime: Date;
  endTime: Date;
  totalLength: number;
};

export type Segment = {
  id?: string;
  name: string;
  length: number;
  coords: LineString;
};

export type ControlPointKind = "start" | "finish" | "cp";

export type ControlPoint = {
  id?: string;
  kind: ControlPointKind;
  name?: string;
  note?: string;
  anchorSegmentId?: string;
  closesAt?: Date;
  coord: Coord;
};
