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
  regions : region_data list;
  map_image : pixbuf;
}

(*First stage of map generation; read in bitmap from filename, go to pixel array*)
val mapimage_to_pixels : string -> color array array

(*initialize the map from two imput files*)
val init_map : color array array -> region_data list -> game_map

(*find the region located at a particular pixel coord*)
val lookup_region : (int * int) -> region_id