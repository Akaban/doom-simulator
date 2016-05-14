exception NotAnAction
val drawSegment : Segment.t -> unit
val draw2D : Bsp.t -> int ->  unit
val drawCollisionZone : Segment.t -> unit
val debugKeys : char -> Player.t -> Bsp.t -> Options.runningData -> unit
val debugKeys2D : char -> Player.t -> Bsp.t -> unit
val debugKeys3D : char -> Player.t -> Bsp.t -> Options.runningData -> unit
val pauseGame : char -> unit
