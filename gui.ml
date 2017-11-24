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

let mymutex = Core.Mutex.create ()

let button_handler name (event: GdkEvent.Button.t) =
  Mutex.lock mymutex;
  print_endline ("Region: " ^ name);
  Mutex.unlock mymutex;
  true

let add_button (pack:GPack.fixed) x y name extra = 
  let button = GButton.button ~label:"0"
  ~packing:(pack#put ~x:x ~y:y) () in
  button#misc#set_name name;
  GtkData.Tooltips.set_tip (GtkData.Tooltips.create ()) button#as_widget 
                          ~text:name ~privat:extra;
  let button_signal = button#event#connect#button_press 
                          ~callback: (button_handler name) in
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
  let log_view = GText.view ~buffer:log_buffer ~editable:false ~width:1590 
                            ~height:300 ~packing:log_pack#add () in
  log_buffer#set_text "Gameplay Log:\ntest";

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
  add_button gameplay_pack 68 60 "Alaska" "Part of North America";
  add_button gameplay_pack 137 110 "Alberta" "Part of North America";
  add_button gameplay_pack 133 245 "Centrial America" "Part of North America";
  add_button gameplay_pack 204 183 "Eastern US" "Part of North America";
  add_button gameplay_pack 427 33 "Greenland" "Part of North America";
  add_button gameplay_pack 175 65 "Northwest Territory" "Part of North America";
  add_button gameplay_pack 225 115 "Ontario" "Part of North America";
  add_button gameplay_pack 302 113 "Quebec" "Part of North America";
  add_button gameplay_pack 120 170 "Western US" "Part of North America";

  (*Final window configuration and display*)
  window#add_accel_group accel_group;
  window#show ();
  let pmap = GdkPixbuf.create_pixmap map_pixbuf |> fst in
  Gdk.Window.set_back_pixmap gameplay_pack#misc#window (`PIXMAP pmap);

  (*main GTK loop*)
  Main.main ()

let () = main ()