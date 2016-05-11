val grey : Graphics.color
val coral : Graphics.color
val bluesky : Graphics.color

val current : Graphics.color ref
val previous : Graphics.color option ref
val set_color : Graphics.color -> unit
val revert_color : unit -> unit
