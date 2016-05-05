val collisionPlayer : Point.t -> Segment.t -> (Segment.tpos * Segment.tpos * Segment.tpos * Segment.tpos)
val detect_collision : Point.t -> Bsp.t -> Segment.t option
