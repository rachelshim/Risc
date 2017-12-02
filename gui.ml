open GMain
open GdkKeysyms
open Gtk
open Unix
open Thread

(*Globals setup*)
let locale = GtkMain.Main.init ()
let buttons_list = ref []
let map_pixbuf = GdkPixbuf.from_file "resources/map.png"
let log_buffer = GText.buffer ()
let log_window_global = ref (GBin.scrolled_window ())

let mymutex = Core.Mutex.create ()

let write_log (message : string) = 
  let old_text = log_buffer#get_text () in
  let new_text = (old_text ^ "\n> ") ^ message in
  log_buffer#set_text new_text;
  let current_adj = !log_window_global#vadjustment in
  current_adj#set_value current_adj#upper;
  !log_window_global#set_vadjustment current_adj;
  ()

let button_handler name (button: GButton.button) (event: GdkEvent.Button.t) =
  Mutex.lock mymutex;
  let sty = button#misc#style#copy in
  sty#set_bg[`NORMAL,`NAME "green"; `INSENSITIVE,`NAME "green" ; 
    `NORMAL,`NAME "green" ; `PRELIGHT,`NAME "green" ; `SELECTED,`NAME "green"];
  button#misc#set_style sty;
  write_log ("Region: " ^ name);
  Mutex.unlock mymutex;
  true
  
let add_button (pack:GPack.fixed) x y name extra = 
  let button = GButton.button ~label:"0"
  ~packing:(pack#put ~x:x ~y:y) () in
  button#misc#set_name name;
  GtkData.Tooltips.set_tip (GtkData.Tooltips.create ()) button#as_widget 
                          ~text:name ~privat:extra;
  let button_signal = button#event#connect#button_press 
                          ~callback: (button_handler name button) in
  buttons_list := (name,button)::(!buttons_list)

let main () =
  let window = GWindow.window ~width:1600 ~height:860
                              ~title:"Risc" ~resizable:false () in
  let window_exit_signal = window#connect#destroy ~callback:Main.quit; in

  let top_pane_pack = GPack.paned ~width:1600 ~height:860 
                              ~packing:window#add ~border_width:5 
                              `HORIZONTAL () in

  let bottom_pane_pack = GPack.paned ~width:1230 ~height:650 
                  ~packing:(top_pane_pack#pack1 ~resize:false ~shrink:false) 
                  ~border_width:5 `VERTICAL () in
  
  let sidebar_pack = GPack.paned ~width:370 ~height:850 ~border_width:5 
                  ~packing:(top_pane_pack#pack2 ~resize:false ~shrink:false)
                  `VERTICAL () in

  let gameplay_pack = GPack.fixed ~has_window:true ~width:1227 ~height:640 
                  ~packing:(bottom_pane_pack#pack1 ~resize:false ~shrink:false) () in

  let log_pack = GPack.vbox ~width:1230 ~height:210 
                  ~packing:(bottom_pane_pack#pack2 ~resize:false ~shrink:false)
                  () in

  let info_pack = GPack.vbox ~width:360 ~height:240 
                  ~packing:(sidebar_pack#pack1 ~resize:false ~shrink:false)
                  () in

  let cards_pack = GPack.vbox ~width:360 ~height:400 
                  ~packing:(sidebar_pack#pack2 ~resize:false ~shrink:false)
                  () in

  (*Game log setup*)
  let log_window = GBin.scrolled_window ~width:1590 ~height:300 ~border_width:0
                            ~packing:log_pack#add () in
  log_window_global := log_window;
  let log_view = GText.view ~buffer:log_buffer ~editable:false ~width:1590 
                            ~height:300 ~packing:log_window#add () in
  log_buffer#set_text "Gameplay Log:";

  (*Menu bar creation*)
  let menubar = GMenu.menu_bar ~packing:(gameplay_pack#add) () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (*File menu setup*)
  let factory = new GMenu.factory file_menu ~accel_group in
  let file_quit_signal = factory#add_item "Quit" ~key:_Q 
                                ~callback: Main.quit in

  (*Region button setup*)
  add_button gameplay_pack 68 60 "Alaska" "North America";
  add_button gameplay_pack 137 110 "Alberta" "North America";
  add_button gameplay_pack 133 245 "Central America" "North America";
  add_button gameplay_pack 204 183 "Eastern US" "North America";
  add_button gameplay_pack 427 33 "Greenland" "North America";
  add_button gameplay_pack 175 65 "Northwest Territory" "North America";
  add_button gameplay_pack 225 115 "Ontario" "North America";
  add_button gameplay_pack 302 113 "Quebec" "North America";
  add_button gameplay_pack 120 170 "Western US" "North America";

  add_button gameplay_pack 290 500 "Argentina" "South America";
  add_button gameplay_pack 332 405 "Brazil" "South America";
  add_button gameplay_pack 224 390 "Peru" "South America";
  add_button gameplay_pack 256 320 "Venezuela" "South America";

  add_button gameplay_pack 505 115 "Great Britain" "Europe";
  add_button gameplay_pack 490 83 "Iceland" "Europe";
  add_button gameplay_pack 590 116 "Northern Europe" "Europe";
  add_button gameplay_pack 585 58 "Scandinavia" "Europe";
  add_button gameplay_pack 631 156 "Southern Europe" "Europe";
  add_button gameplay_pack 678 108 "Ukraine" "Europe";
  add_button gameplay_pack 553 156 "Western Europe" "Europe";

  add_button gameplay_pack 633 354 "Congo" "Africa";
  add_button gameplay_pack 710 319 "East Africa" "Africa";
  add_button gameplay_pack 635 235 "Egypt" "Africa";
  add_button gameplay_pack 754 450 "Madagascar" "Africa";
  add_button gameplay_pack 545 275 "North Africa" "Africa";
  add_button gameplay_pack 638 447 "South Africa" "Africa";

  add_button gameplay_pack 793 152 "Afghanistan" "Asia";
  add_button gameplay_pack 983 216 "China" "Asia";
  add_button gameplay_pack 863 246 "India" "Asia";
  add_button gameplay_pack 940 105 "Irkutsk" "Asia";
  add_button gameplay_pack 1105 187 "Japan" "Asia";
  add_button gameplay_pack 1065 70 "Kamchatka" "Asia";
  add_button gameplay_pack 716 202 "Middle East" "Asia";
  add_button gameplay_pack 957 153 "Mongolia" "Asia";
  add_button gameplay_pack 970 276 "Siam" "Asia";
  add_button gameplay_pack 860 61 "Siberia" "Asia";
  add_button gameplay_pack 784 78 "Ural" "Asia";
  add_button gameplay_pack 955 56 "Yakutsk" "Asia";

  add_button gameplay_pack 1140 472 "Eastern Australia" "Australia";
  add_button gameplay_pack 1016 352 "Indonesia" "Australia";
  add_button gameplay_pack 1140 377 "New Guinea" "Australia";
  add_button gameplay_pack 1036 473 "Western Australia" "Australia";

  (*Final window configuration and display*)
  window#add_accel_group accel_group;
  window#show ();
  let pmap = GdkPixbuf.create_pixmap map_pixbuf |> fst in
  Gdk.Window.set_back_pixmap gameplay_pack#misc#window (`PIXMAP pmap);

  (*main GTK loop*)
  Main.main ()

let () = main ()