type tmode = TwoD | ThreeD | TwoDdebug

val mode : tmode

val cin : in_channel

val win_w : int
val win_h : int

val ceiling_h : int
val floor_h : int
val wall_h : int
val eye_h : int

val fov : int

val bg : Graphics.color
val fill_color : Graphics.color
val contour_color : Graphics.color
val draw_contour : bool ref
val fill_wall : bool ref

val mouse_sensitivity : int
val angular_change : int

val max_dist : float

val step_dist : float

val size2d : int

val wall_collision_size : float

val angleMiniMap : int
val sizeAngleMiniMap : float

val xmin : float
val xmax : float

val scale : int
val minimap : bool
 
val debug_manualRender : bool ref

val debug : bool ref
val debug_bsp : bool

val collision : bool ref
