type t = {
  id : int;
  porig : Point.t; 
  pdest : Point.t;
  ci : float;
  ce : float
}

type tpos = L | R | C

val new_segment : int -> int -> int -> int -> t

val real_coord : t -> (float*float) * (float*float)

val get_position : Point.t -> t -> tpos

val split_segment : t -> t -> t option * t option

val split : t -> t list -> t list * t list

val angle : t -> int
