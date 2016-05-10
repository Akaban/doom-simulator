type t = {
  mutable pos : Point.t;
  mutable pa : int;
  mutable oldpos : Point.t;
  mutable rAngleMinimap : Segment.t;
  mutable lAngleMinimap : Segment.t

}

val new_player : Point.t -> int -> t

val calculateAngleMinimapS : Point.t -> int -> bool -> (Segment.t*Segment.t)

type dir = Left | Right

val rotate : dir -> t -> unit

type mv = MFwd | MBwd | MLeft | MRight

val move : mv -> t -> Bsp.t -> unit


