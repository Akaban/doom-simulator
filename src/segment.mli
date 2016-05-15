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
  sens : tpos;
  couleur : Graphics.color option
}

val updateColor : t -> Graphics.color -> t

val resetIdCount : unit -> unit

val bottomRight : t -> Point.t

val tposToString : tpos -> string

val toString : t -> string

val new_segment : int -> int -> int -> int -> t
val new_segmentSimple : int -> int -> int -> int -> t
val new_segmentSimpleFloat : float -> float -> float -> float -> t
val new_segmentPoint : Point.t -> Point.t -> t
val new_segmentPointSimple : Point.t -> Point.t -> t
val new_segmentSimpleFloatWithid : float -> float -> float -> float -> int -> t

val originVector : t -> (int*int)

val rotateSegmentOrig : t -> int -> t

val fromSome : 'a -> 'a option -> 'a

val real_coord : t -> (float*float) * (float*float)
val real_coordInt :t -> (int*int) * (int * int)

val norme : t -> int

val get_position : Point.t -> t -> tpos

val get_position_s : Point.t -> t -> string


val split : t -> t list -> t list * t list
val splitWithoutId : t -> t list -> t list * t list

val angle : t -> float
val tangle : t -> float
val tangleTuple : (float*float*float*float) -> float
