open GMain
open GdkKeysyms
open Gtk
open Unix
open Thread

(*Selection modes*)
type selection_mode = 
  | No_selection
  | Single
  | Double

(*Globals setup*)
let color_options = ["Red"; "Blue"; "Green"; "Yellow"; "Purple"; "Orange"]
let actions_strings =   ["Deploy"; "Attack"; "Reinforce"; "Move"; 
                         "Trade Cards - 3 Same"; "Trade Cards - 3 Different"; 
                         "End turn"]
let locale = GtkMain.Main.init ()
let continent_labels_list = ref []
let buttons_list = ref []
let territory_troop_list = ref []
let map_pixbuf = GdkPixbuf.from_file "resources/map.png"
let log_buffer = GText.buffer ()
let log_window_global = ref (GBin.scrolled_window ())
let player_label_global = ref (GMisc.label ())
let reinforcement_label_global = ref (GMisc.label ())
let infantry_label_global = ref (GMisc.label ())
let calvalry_label_global = ref (GMisc.label ())
let artillery_label_global = ref (GMisc.label ())
let wildcard_label_global = ref (GMisc.label ())
let territories_label_global = ref (GMisc.label ())
let troops_label_global = ref (GMisc.label ())
let selection1_label_global = ref (GMisc.label ())
let selection2_label_global = ref (GMisc.label ())
let confirm_button_global = ref (GButton.button ())
let cards_cbox_global = ref (fst (GEdit.combo_box_text ()))
let actions_cbox_global = ref (fst (GEdit.combo_box_text ()))

let current_selection_mode = ref No_selection
let selection1 = ref None
let selection2 = ref None

let mutex = Core.Mutex.create ()

let set_color wid col_str = 
  let sty = wid#misc#style#copy in
  sty#set_bg[`NORMAL,`NAME col_str; `INSENSITIVE,`NAME col_str; 
    `NORMAL,`NAME col_str; `PRELIGHT,`NAME col_str; `SELECTED,`NAME col_str];
  wid#misc#set_style sty;
  ()

let lookup_troop_count name = 
  List.assoc name !territory_troop_list

let set_territory_sensitivity name new_sens = 
  let button = List.assoc name !buttons_list in
  button#misc#set_sensitive new_sens;
  ()

let set_territory_buttons_sensitivity new_sens = 
  let buttons = snd (List.split !buttons_list) in
  let u_list = List.map (fun b -> b#misc#set_sensitive new_sens) buttons in
  ()

let clear_selections () = 
  selection1 := None;
  selection2 := None;
  !selection1_label_global#set_text 
    ("No Selection");
  !selection2_label_global#set_text 
    ("No Selection");
  ()

(*
 * [make_selection name] sets the global territory selection set based
 * on the current state of the set and the current global selection mode.
 * For single selection settings, selection1 and its associated label are set
 * if no selection already exists. for double selection settings, selection1 
 * is set first if it is not already set, followed by selection2 if selection1
 * contains a selection. For No_selection mode, this method has no effect.
 * If possible, selects the territory specified by [name], and returns true if
 * successful, false if not.
 *)
let make_selection name = 
  match !current_selection_mode with
  | No_selection -> false
  | Single -> begin
    match !selection1 with
    | None -> selection1 := Some name;
            selection2 := None;
            !selection1_label_global#set_text name;
            !selection2_label_global#set_text "No Selection";
              set_territory_buttons_sensitivity false;
            true
    | Some _ -> false
  end
  | Double -> begin
    match !selection1 with
    | None -> selection1 := Some name;
              selection2 := None;
              !selection1_label_global#set_text name;
              !selection2_label_global#set_text "No Selection";
              set_territory_sensitivity name false;
              true
    | Some _ -> begin
      match !selection2 with
      | None -> selection2 := Some name;
                !selection2_label_global#set_text name;
                  set_territory_buttons_sensitivity false;
                true
      | Some _ -> false
    end
  end

let lock_all () =
  (*lock territories, clear selection, lock confirm *)
  set_territory_buttons_sensitivity false;
  clear_selections ();
  !confirm_button_global#misc#set_sensitive false;
  ()

let set_selection_mode mode = 
  match mode with
  | No_selection -> set_territory_buttons_sensitivity false;
                    current_selection_mode := mode
  | Single -> set_territory_buttons_sensitivity true;
              current_selection_mode := mode
  | Double -> set_territory_buttons_sensitivity true;
              current_selection_mode := mode

(* EXPOSED SETTER METHODS BEGIN *)

let set_territory_troops name num = 
  let button = List.assoc name !buttons_list in
  territory_troop_list := List.remove_assoc name !territory_troop_list;
  territory_troop_list := (name, num)::!territory_troop_list;
  button#set_label (string_of_int num);
  ()

let rec update_territories (data:(string * string * int) list) = 
  match data with
  | [] -> ()
  | (name, owner, troops)::tl -> begin
    let button = List.assoc name !buttons_list in
    set_color button owner;
    button#set_label (string_of_int troops);
    update_territories tl
  end

let rec update_continent_owners (data:(string * string) list) = 
  match data with
  | [] -> ()
  | (c_name, owner)::tl -> begin
    let frame = List.assoc c_name !continent_labels_list in
    set_color frame owner;
    update_continent_owners tl
  end

let update_current_player player = 
  !player_label_global#set_text ("Current Player: " ^ player);
  ()

let update_available_reinforcements num = 
  !reinforcement_label_global#set_text 
    ("Reinforcements Available: " ^ (string_of_int num));
  ()

let update_cards (inf, cav, art, wild) = 
  !infantry_label_global#set_text 
    ("Infantry Cards: " ^ (string_of_int inf));
  !calvalry_label_global#set_text 
    ("Calvalry Cards: " ^ (string_of_int cav));
  !artillery_label_global#set_text 
    ("Artillery Cards: " ^ (string_of_int art));
  !wildcard_label_global#set_text 
    ("Wildcards: " ^ (string_of_int wild));
  ()

let update_territories_count count = 
  !territories_label_global#set_text 
    ("Territories Controlled: " ^ (string_of_int count));
  ()

let update_troop_count count = 
  !troops_label_global#set_text 
    ("Troops Deployed: " ^ (string_of_int count));
  ()

let write_log (message : string) = 
  let old_text = log_buffer#get_text () in
  let new_text = old_text ^ ("\n> " ^ message) in
  log_buffer#set_text new_text;
  let current_adj = !log_window_global#vadjustment in
  current_adj#set_value current_adj#upper;
  !log_window_global#set_vadjustment current_adj;
  ()

let set_game_over over = 
  if over then begin
    lock_all ();
    write_log "Game ended.";
    ()
  end
  else ()

(* EXPOSED SETTER METHODS END *)

let actions_cbox_handler (box: GEdit.combo_box GEdit.text_combo) () = 
  Mutex.lock mutex;
  let index = (fst box)#active in
  (*disallow illegal arguments*)
  if index >= 0 || index >= List.length actions_strings then begin
    let sel = List.nth actions_strings index in
    write_log ("Selected Action: " ^ sel);
    clear_selections ();
    !confirm_button_global#misc#set_sensitive true;
    if sel = "Deploy" then begin
      set_selection_mode Single
    end
    else if sel = "Attack" then begin
      set_selection_mode Double
    end
    else if sel = "Reinforce" then begin
      set_selection_mode Single
    end
    else if sel = "Move" then begin
      set_selection_mode Double
    end
    else begin
      set_selection_mode No_selection
    end;
    (*card trading in separate if-clause because all others should lock it*)
    if sel = "Trade Cards - 3 Same" then begin
      !cards_cbox_global#misc#set_sensitive true
    end
    else begin
      !cards_cbox_global#misc#set_sensitive false
    end
  end
  else ();
  Mutex.unlock mutex;
  ()

let territory_button_handler name (button: GButton.button) () =
  Mutex.lock mutex;
  set_color button "Blue";
  set_territory_troops name (lookup_troop_count name |> succ);
  write_log ("Region: " ^ name);
  let sel_result = make_selection name in
  Mutex.unlock mutex;
  ()

let cancel_button_handler () = 
  Mutex.lock mutex;
  clear_selections ();
  set_territory_buttons_sensitivity true;
  Mutex.unlock mutex;
  ()

let confirm_button_handler () = 
  Mutex.lock mutex;
  print_endline "confirm button";
  Mutex.unlock mutex;
  ()

let run_init_dialog parent = 
  let players_num = ref None in
  let init_dialog_accept_handler cbox dialog () = 
    let num = ((fst cbox)#active + 2) in
    let res = dialog#event#send (GdkEvent.create `DELETE) in
    dialog#destroy ();
    if num = 1 then (players_num := None)
    else (players_num := Some num);
    ()
  in
  let init_dialog = GWindow.dialog ~parent:parent ~destroy_with_parent:true 
                  ~title:"Initialization Dialog" ~deletable:true 
                  ~resizable:false () in
  let init_dialog_label = GMisc.label 
                  ~text:"Please select the number of players."
                  ~packing:init_dialog#vbox#add () in
  let init_dialog_options = ["2";"3";"4";"5";"6"] in
  let init_dialog_combobox = GEdit.combo_box_text 
                  ~strings:init_dialog_options
                  (* ~width:100 ~height:20 *)
                  ~packing:init_dialog#vbox#add () in
  let init_dialog_accept_button = GButton.button ~label:"Accept"
                  ~packing:init_dialog#vbox#add () in
  let accept_signal = 
      init_dialog_accept_button#connect#clicked 
      ~callback:(init_dialog_accept_handler init_dialog_combobox init_dialog) in
  let close_event = init_dialog#event#connect#delete 
      ~callback:(fun _ -> init_dialog#destroy (); true) in
  let init_dialog_delete_event = init_dialog#run () in
  !players_num
  
let run_troop_dialog parent message (min, max) = 
  let value = ref None in
  let troop_dialog_accept_handler scale dialog () = 
    let num = int_of_float scale#adjustment#value in
    let res = dialog#event#send (GdkEvent.create `DELETE) in
    dialog#destroy ();
    value := Some num;
    ()
  in
  let fmin = float_of_int min in
  let fmax = float_of_int max in
  let troop_dialog = GWindow.dialog ~parent:parent ~destroy_with_parent:true 
                  ~title:"Troop Selection" ~deletable:true 
                  ~resizable:false () in
  let troop_dialog_label = GMisc.label 
                  ~text:message
                  ~packing:troop_dialog#vbox#add () in
  let troop_dialog_adjustment = GData.adjustment ~value:fmin
                  ~lower:fmin ~upper:fmax () in
  let scale_frame = GBin.frame ~width:200 ~height:50 ~border_width:3
                  ~packing:troop_dialog#vbox#add () in
  let troop_dialog_scale = GRange.scale `HORIZONTAL 
                  ~draw_value:true ~digits:0 ~update_policy:`CONTINUOUS
                  ~adjustment:troop_dialog_adjustment 
                  ~packing:scale_frame#add () in
  let troop_dialog_accept_button = GButton.button ~label:"Accept"
                  ~packing:troop_dialog#vbox#add () in
  let accept_signal = 
    troop_dialog_accept_button#connect#clicked 
      ~callback:(troop_dialog_accept_handler troop_dialog_scale troop_dialog) in
  let close_event = troop_dialog#event#connect#delete 
      ~callback:(fun _ -> troop_dialog#destroy (); true) in
  let init_dialog_delete_event = troop_dialog#run () in
  !value

let add_territory (pack:GPack.fixed) x y name extra = 
  let button = GButton.button ~label:"0"
                              ~packing:(pack#put ~x:x ~y:y) () in
  button#misc#set_name name;
  GtkData.Tooltips.set_tip (GtkData.Tooltips.create ()) button#as_widget 
                          ~text:name ~privat:extra;
  let button_signal = button#connect#clicked
                          ~callback: (territory_button_handler name button) in                          
  buttons_list := (name,button)::(!buttons_list);
  territory_troop_list := (name, 0)::(!territory_troop_list);
  ()

let add_label (pack:GPack.fixed) x y width height name = 
  let label_frame = GBin.frame ~width:width ~height:height 
                               ~packing:(pack#put ~x:x ~y:y) () in
  let label = GMisc.label ~text: name 
                          ~packing:label_frame#add () in
  continent_labels_list := (name, label_frame)::(!continent_labels_list);
  set_color label_frame "grey";
  ()

let main () =
  let window = GWindow.window ~width:1500 ~height:860
                              ~title:"Risc" ~resizable:false () in
  let window_exit_signal = window#connect#destroy ~callback:Main.quit in

  let top_pane_pack = GPack.paned ~width:1500 ~height:860 
                              ~packing:window#add ~border_width:5 
                              `HORIZONTAL () in

  let bottom_pane_pack = GPack.paned ~width:1230 ~height:650 
                  ~packing:(top_pane_pack#pack1 ~resize:false ~shrink:false) 
                  ~border_width:5 `VERTICAL () in
  
  let sidebar_pack = GPack.paned ~width:270 ~height:850 ~border_width:5 
                  ~packing:(top_pane_pack#pack2 ~resize:false ~shrink:false)
                  `VERTICAL () in

  let gameplay_frame = GBin.frame ~label:"Game Map" ~width:1229 ~height:642 
                  ~packing:(bottom_pane_pack#pack1 ~resize:false ~shrink:false)
                  ~border_width:1 () in

  let gameplay_pack = GPack.fixed ~has_window:true ~width:1227 ~height:640 
                  ~packing:gameplay_frame#add () in

  let log_frame = GBin.frame ~label:"Gameplay Log" ~width:1230 ~height:210 
                  ~packing:(bottom_pane_pack#pack2 ~resize:false ~shrink:false)
                  () in

  let info_frame = GBin.frame ~label:"Information"
                  ~packing:(sidebar_pack#pack1 ~resize:false ~shrink:false)
                  () in

  let info_pack = GPack.vbox ~width:260 ~height:240 
                  ~packing:info_frame#add
                  () in
  
  let actions_frame = GBin.frame ~label:"Actions"
                  ~packing:(sidebar_pack#pack2 ~resize:false ~shrink:false)
                  () in

  let actions_pack = GPack.vbox ~width:260 ~height:400 
                  ~packing:actions_frame#add () in

  (*Get number of players via dialog box*)
  let got_player_num = ref false in
  let player_num = ref 0 in
  while not (!got_player_num) do
    let init_result= run_init_dialog window in
    match init_result with
    | Some x -> player_num := x;
                got_player_num := true;
    | _ -> ()
  done;

  (*Info pack setup*)
  let player_label = GMisc.label ~text:"Current Player: N/A"
                                 ~packing:info_pack#add () in
  player_label_global := player_label;

  let reinforcement_label = GMisc.label ~text:"Reinforcements Available: 0"
                                 ~packing:info_pack#add () in
  reinforcement_label_global := reinforcement_label;

  let infantry_label = GMisc.label ~text:"Infantry Cards: 0"
                                 ~packing:info_pack#add () in
  infantry_label_global := infantry_label;

  let calvalry_label = GMisc.label ~text:"Calvalry Cards: 0"
                                 ~packing:info_pack#add () in
  calvalry_label_global := calvalry_label;

  let artillery_label = GMisc.label ~text:"Artillery Cards: 0"
                                 ~packing:info_pack#add () in
  artillery_label_global := artillery_label;

  let wildcard_label = GMisc.label ~text:"Wildcards: 0"
                                 ~packing:info_pack#add () in
  wildcard_label_global := wildcard_label;

  let territories_label = GMisc.label ~text:"Territories Controlled: 0"
                                 ~packing:info_pack#add () in
  territories_label_global := territories_label;

  let troops_label = GMisc.label ~text:"Troops Deployed: 0"
                                 ~packing:info_pack#add () in
  troops_label_global := troops_label;

  (*Action pack setup*)
  
  let actions_cbox_frame = GBin.frame ~label:"Move Selection" ~border_width:3
                  ~packing:actions_pack#add () in
  let actions_cbox = GEdit.combo_box_text 
              ~strings:actions_strings
              ~width:100 ~height:20 
              ~packing:actions_cbox_frame#add () in
  let actions_signal = (fst actions_cbox)#connect#changed 
                        (actions_cbox_handler actions_cbox) in
  actions_cbox_global := fst actions_cbox;

  let cards_cbox_frame = GBin.frame ~label:"Card Selection" ~border_width:3
                  ~packing:actions_pack#add () in
  let cards_cbox = GEdit.combo_box_text 
              ~strings:["Infantry";"Calvalry";"Artillery"]
              ~width:100 ~height:20 
              ~packing:cards_cbox_frame#add () in
  cards_cbox_global := fst cards_cbox;

  let confirm_button = GButton.button ~label:"Confirm"
                                      ~packing:actions_pack#add () in
  let confirm_button_signal = confirm_button#connect#clicked 
                                      (confirm_button_handler) in
  confirm_button_global := confirm_button;

  let cancel_button = GButton.button ~label:"Cancel"
                                      ~packing:actions_pack#add () in
  let cancel_button_signal = 
    cancel_button#connect#clicked cancel_button_handler in
  
  let selection1_frame = GBin.frame ~label:"Territory Selection 1" 
                                    ~border_width:3 
                                    ~packing:actions_pack#add () in
  let selection1_label = GMisc.label ~text:"No Selection"
                                     ~packing:selection1_frame#add () in         
  selection1_label_global := selection1_label;

  let selection2_frame = GBin.frame ~label:"Territory Selection 2" 
                                    ~border_width:3 
                                    ~packing:actions_pack#add () in
  let selection2_label = GMisc.label ~text:"No Selection"
                                     ~packing:selection2_frame#add () in
  selection2_label_global := selection2_label;

  (*Game log setup*)
  let log_window = GBin.scrolled_window ~width:1590 ~height:300 ~border_width:0
                            ~packing:log_frame#add () in
  log_window_global := log_window;
  let log_view = GText.view ~buffer:log_buffer ~editable:false ~width:1590 
                            ~height:300 ~packing:log_window#add () in
  log_buffer#set_text 
    ("> Game started with " ^ (string_of_int !player_num) ^ " players.");

  (*Menu bar creation*)
  let menubar = GMenu.menu_bar ~packing:(info_pack#add) () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (*File menu setup*)
  let factory = new GMenu.factory file_menu ~accel_group in
  let file_quit_signal = factory#add_item "Quit" ~key:_Q 
                                ~callback: Main.quit in

  (*Continent label setup*)
  add_label gameplay_pack 274 204 110 25 "North America";
  add_label gameplay_pack 138 445 110 25 "South America";
  add_label gameplay_pack 602 25 60 25 "Europe";
  add_label gameplay_pack 530 355 60 25 "Africa";
  add_label gameplay_pack 1056 231 55 25 "Asia";
  add_label gameplay_pack 990 415 70 25 "Australia";

  (*Region button setup*)
  add_territory gameplay_pack 68 60 "Alaska" "North America";
  add_territory gameplay_pack 137 110 "Alberta" "North America";
  add_territory gameplay_pack 133 245 "Central America" "North America";
  add_territory gameplay_pack 204 183 "Eastern United States" "North America";
  add_territory gameplay_pack 427 33 "Greenland" "North America";
  add_territory gameplay_pack 175 65 "Northwest Territory" "North America";
  add_territory gameplay_pack 225 115 "Ontario" "North America";
  add_territory gameplay_pack 302 113 "Quebec" "North America";
  add_territory gameplay_pack 120 170 "Western United States" "North America";

  add_territory gameplay_pack 290 500 "Argentina" "South America";
  add_territory gameplay_pack 332 405 "Brazil" "South America";
  add_territory gameplay_pack 224 390 "Peru" "South America";
  add_territory gameplay_pack 256 320 "Venezuela" "South America";

  add_territory gameplay_pack 505 115 "Great Britain" "Europe";
  add_territory gameplay_pack 490 83 "Iceland" "Europe";
  add_territory gameplay_pack 590 116 "Northern Europe" "Europe";
  add_territory gameplay_pack 585 58 "Scandinavia" "Europe";
  add_territory gameplay_pack 631 156 "Southern Europe" "Europe";
  add_territory gameplay_pack 678 108 "Ukraine" "Europe";
  add_territory gameplay_pack 553 156 "Western Europe" "Europe";

  add_territory gameplay_pack 633 354 "Congo" "Africa";
  add_territory gameplay_pack 710 319 "East Africa" "Africa";
  add_territory gameplay_pack 635 235 "Egypt" "Africa";
  add_territory gameplay_pack 754 450 "Madagascar" "Africa";
  add_territory gameplay_pack 545 275 "North Africa" "Africa";
  add_territory gameplay_pack 638 447 "South Africa" "Africa";

  add_territory gameplay_pack 793 152 "Afghanistan" "Asia";
  add_territory gameplay_pack 983 216 "China" "Asia";
  add_territory gameplay_pack 863 246 "India" "Asia";
  add_territory gameplay_pack 940 105 "Irkutsk" "Asia";
  add_territory gameplay_pack 1105 187 "Japan" "Asia";
  add_territory gameplay_pack 1065 70 "Kamchatka" "Asia";
  add_territory gameplay_pack 716 202 "Middle East" "Asia";
  add_territory gameplay_pack 957 153 "Mongolia" "Asia";
  add_territory gameplay_pack 970 276 "Siam" "Asia";
  add_territory gameplay_pack 860 61 "Siberia" "Asia";
  add_territory gameplay_pack 784 78 "Ural" "Asia";
  add_territory gameplay_pack 955 56 "Yakutsk" "Asia";

  add_territory gameplay_pack 1140 472 "Eastern Australia" "Australia";
  add_territory gameplay_pack 1016 352 "Indonesia" "Australia";
  add_territory gameplay_pack 1140 377 "New Guinea" "Australia";
  add_territory gameplay_pack 1036 473 "Western Australia" "Australia";

  (*
  let names = fst (List.split !buttons_list) in
  let st = String.concat "; " names in
  print_endline st;
  *)

  (*
  let fres = run_troop_dialog window "message" (1, 40) in
  match fres with
  | None -> print_endline "none"
  | Some x -> print_endline ("some: " ^ string_of_int x);
  *)

  (*Set some sensitivities before game starts*)
  set_territory_buttons_sensitivity false;
  (fst cards_cbox)#misc#set_sensitive false;
  confirm_button#misc#set_sensitive false;

  (*Final window configuration and display*)
  window#add_accel_group accel_group;
  window#show ();
  let pmap = GdkPixbuf.create_pixmap map_pixbuf |> fst in
  Gdk.Window.set_back_pixmap gameplay_pack#misc#window (`PIXMAP pmap);

  (*main GTK loop*)
  Main.main ()

let () = main ()