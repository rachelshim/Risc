open Graphics
open GdkPixbuf

type region_id = string

type region_data = {
  id : region_id;
  text_location : int * int;
  color : color;
  adjacencies : region_id list;
  continent : string; (*possibly change later*)
}

type game_map = {
  selected_1 : region_id option;
  selected_2 : region_id option;
  regions : region_data list;
  map_image_original: pixbuf;
  map_image_drawn: pixbuf;
}

(*First stage of map generation; read in bitmap from filename, go to pixel array*)
val mapimage_to_pixels : string -> color array array

(*initialize the map from two imput files*)
val init_map : color array array -> region_data list -> game_map

(*find the region located at a particular pixel coord*)
val lookup_region : (int * int) -> region_id

(*draws the specified string over the requested region*)
val draw_string : region_id -> string -> game_map

(*resets the specified game map to its undrawn state*)
val reset_map_image : game_map -> game_map