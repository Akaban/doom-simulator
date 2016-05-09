type t = {x : int; y : int}

val new_point : int -> int -> t

val divPoint : t -> int -> t

val toString : t -> string

val translateVect : (float*float) -> int -> t
val translatePoint : t -> t -> t
val translatePointWithAngle : t -> (float*float) -> int -> t
