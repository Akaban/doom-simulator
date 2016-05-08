type tmode = TwoD | ThreeD

val mode : tmode

val cin : in_channel

val win_w : int
val win_h : int

val ceiling_h : int
val floor_h : int
val wall_h : int

val fov : int

val bg : Graphics.color

val mouse_sensitivity : int
val angular_change : int

val max_dist : int

val step_dist : float

val size2d : int

val wall_collision_size : int

val xmin : float
val xmax : float

val scale : int
val minimap : bool
 
val debug : bool ref
val debug_bsp : bool

val collision : bool ref
