type t = {x : int; y : int}

val new_point : int -> int -> t

val distance : t -> t -> float

val divPoint : t -> int -> t

val toString : t -> string

val translateVect : (float*float) -> int -> t
val translatePoint : t -> t -> t
val translatePointWithAngle : t -> (float*float) -> int -> t
val translatePointFloatWithAngle : (float*float) -> (float*float) -> int -> (float*float)
val translateVectFloat : (float*float) -> int -> (float*float)
