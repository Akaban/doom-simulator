type tpos = L | R | C

type t = {
  id : int;
  porig : Point.t; 
  pdest : Point.t;
  ci : float;
  ce : float;
  segBottom: t option;
  segRight : t option;
  segTop : t option;
  segLeft : t option;
  sens : tpos
}

val bottomRight : t -> Point.t



val tposToString : tpos -> string

val toString : t -> string

val new_segment : int -> int -> int -> int -> t
val new_segmentSimple : int -> int -> int -> int -> t

val fromSome : 'a -> 'a option -> 'a

val real_coord : t -> (float*float) * (float*float)
val real_coordInt :t -> (int*int) * (int * int)

val drawSegment : t -> unit

val drawCollisionZone : t -> unit

val get_position : Point.t -> t -> tpos

val get_position_s : Point.t -> t -> string

val split_segment : t -> t -> t option * t option

val split : t -> t list -> t list * t list

val angle : t -> float
val tangle : t -> float
