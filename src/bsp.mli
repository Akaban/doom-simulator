type tint = F | N of int * tint * tint
type t = E | N of Segment.t * t * t

val bspId : t -> tint

val getRight : t -> t
val getLeft : t -> t

val parse : (Segment.t -> 'a) -> t -> Point.t -> unit

val rev_parse : (Segment.t -> 'a) -> t -> Point.t -> unit

val parseLeft : (Segment.t -> 'a) -> t -> Point.t -> unit

val toList : t -> Segment.t list

val iter : (Segment.t -> 'a) -> t -> unit

val build_bsp : Segment.t list -> t

val build_bspWithPivot : Segment.t -> Segment.t list -> t

val print_tree : t -> int -> string

val instanceBsp : t ref

val updateBsp : t -> unit
