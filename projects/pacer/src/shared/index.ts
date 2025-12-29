export type Coord = { lng: number; lat: number };

export enum ControlPointKind {
  Start = "START",
  Finish = "FINISH",
  Control = "CONTROL",
}

export type ControlPoint = {
  kind: ControlPointKind;
  name?: string;
  opensAt?: Date;
  closesAt?: Date;
  coord: Coord;
};
