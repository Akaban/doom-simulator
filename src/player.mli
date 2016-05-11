type t = {
  mutable pos : Point.t;
  mutable pa : int;
  mutable oldpos : Point.t;
  mutable rAngleMinimap : Segment.t;
  mutable lAngleMinimap : Segment.t;
  mutable crouch : bool
}

val new_player : Point.t -> int -> t

val calculateAngleMinimap : Point.t -> int -> (Segment.t*Segment.t)

type dir = Left | Right

val rotate : dir -> t -> unit

val crouchPlayer : t -> unit

type mv = MFwd | MBwd | MLeft | MRight

val tp : (int*int*int) -> t -> Bsp.t -> unit

val move : mv -> t -> Bsp.t -> unit


