open GMain
open Gtk
open Action
open Printexc

(*Selection modes*)
type selection_mode =
  | No_selection
  | Single
  | Double

(*Globals setup*)
(*
 * Note: Ivars don't work for many of these cases because the values contained
 * must be mutated, and overhead caused by having half Ivar and half ref
 * would have been significant.
 *)
let color_options = ["Red"; "Blue"; "Green"; "Yellow"; "Purple"; "Orange"]
let actions_strings =   ["Deploy"; "Attack"; "Reinforce"; "Move";
                         "Trade Cards"; "End turn"]
let cards_strings = ["Infantry";"Cavalry";"Artillery";"Wildcard"]
let locale = GtkMain.Main.init ()
let controller = ref (Controller.init_state_emp 2)
let window_global = ref (GWindow.window ())
let continent_labels_list = ref []
let buttons_list = ref []
let map_pixbuf = GdkPixbuf.from_file "resources/map.png"
let icon_pixbuf = GdkPixbuf.from_file "resources/icon.png"
let log_buffer = GText.buffer ()
let log_window_global = ref (GBin.scrolled_window ())
let player_label_global = ref (GMisc.label ())
let reinforcement_label_global = ref (GMisc.label ())
let infantry_label_global = ref (GMisc.label ())
let cavalry_label_global = ref (GMisc.label ())
let artillery_label_global = ref (GMisc.label ())
let wildcard_label_global = ref (GMisc.label ())
let territories_label_global = ref (GMisc.label ())
let troops_label_global = ref (GMisc.label ())
let selection1_label_global = ref (GMisc.label ())
let selection2_label_global = ref (GMisc.label ())
let confirm_button_global = ref (GButton.button ())
let actions_cbox_global = ref (fst (GEdit.combo_box_text ()))
let current_selection_mode = ref No_selection
let selection1 = ref None
let selection2 = ref None

(*
 * [card_of_int num] is an Action.card option produced by mapping the int value
 * [num] to Some 0:Infantry, 1:Calvalry, 2:Artillery, and 3:Wild. Any other
 * value of [num] yeilds None.
 *)
let card_of_int num =
  if num = 0 then Some Infantry
  else if num = 1 then Some Cavalry
  else if num = 2 then Some Artillery
  else if num = 3 then Some Wild
  else None

(*
 * [string_of_card card] is a string giving a representation of the Action.card
 * described by [card].
 *)
let string_of_card card =
  match card with
  | Infantry -> "Infantry"
  | Cavalry -> "Cavalry"
  | Artillery -> "Artillery"
  | Wild -> "Wildcard"

(*
 * [set_color wid col_str] is a function with the side effect that it sets the
 * color properties of the Gtk widget [wid] to the color [col_str], which must
 * be a member of the list of strings given in /usr/lib/x11/rgb.txt, or the
 * function will have no effect (fails silently in the event of
 * Gdk.Error("color_parse")).
 *)
let set_color wid col_str =
  let sty = wid#misc#style#copy in
  let () =
    try
      sty#set_bg[`NORMAL,`NAME col_str; `INSENSITIVE,`NAME col_str;
        `NORMAL,`NAME col_str;`PRELIGHT,`NAME col_str;`SELECTED,`NAME col_str];
    with e -> ()
  in
  wid#misc#set_style sty;
  ()

(*
 * [set_territory_sensitivity name new_sens] is a function with the side effect
 * that the territory button associated with [name] in buttons_list is set to
 * have the sensitivity specified by [new_sens].
 *)
let set_territory_sensitivity name new_sens =
  let button = List.assoc name !buttons_list in
  button#misc#set_sensitive new_sens;
  ()

(*
 * [set_territory_buttons_sensitivity new_sens] is a function with the side
 * effect that it sets all territory buttons to have the sensitivity [new_sens].
 *)
let set_territory_buttons_sensitivity new_sens =
  let buttons = snd (List.split !buttons_list) in
  ignore(List.map (fun b -> b#misc#set_sensitive new_sens) buttons);
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
  | Single -> begin (*single selection case*)
    match !selection1 with
    | None -> selection1 := Some name;
            selection2 := None;
            !selection1_label_global#set_text name;
            !selection2_label_global#set_text "No Selection";
              set_territory_buttons_sensitivity false;
            true
    | Some _ -> false
  end
  | Double -> begin (*cases for two-selection actions*)
    match !selection1 with
    | None -> selection1 := Some name; (*first isn't selected yet*)
              selection2 := None;
              !selection1_label_global#set_text name;
              !selection2_label_global#set_text "No Selection";
              set_territory_sensitivity name false;
              true
    | Some _ -> begin (*first is already selected*)
      match !selection2 with
      | None -> selection2 := Some name;
                !selection2_label_global#set_text name;
                  set_territory_buttons_sensitivity false;
                true
      | Some _ -> false
    end
  end

(*
 * [set_selection_mode mode] is a function with the side effect that it sets the
 * global selection mode and related sensitivity information based on the value
 * provided in [mode].
 *)
let set_selection_mode mode =
  match mode with
  | No_selection -> set_territory_buttons_sensitivity false;
                    current_selection_mode := mode
  | Single -> set_territory_buttons_sensitivity true;
              current_selection_mode := mode
  | Double -> set_territory_buttons_sensitivity true;
              current_selection_mode := mode

(*
 * [clear_selections ()] is a function with the side effect that it unsets
 * internal tracking of selections, resets GUI territory selection displays
 * and reloads the selection mode.
 *)
let clear_selections () =
  selection1 := None;
  selection2 := None;
  !selection1_label_global#set_text
    ("No Selection");
  !selection2_label_global#set_text
    ("No Selection");
  set_selection_mode !current_selection_mode;
  ()

(*
 * [lock_all ()] is a function with the side effect that is locks the gui into
 * an unusable state by disabling the confirm and territory buttons. This
 * is for use only when the game is over to prevent any action by players.
 *)
let lock_all () =
  (*lock territories, clear selection, lock confirm *)
  set_territory_buttons_sensitivity false;
  clear_selections ();
  !confirm_button_global#misc#set_sensitive false;
  ()


(*
 * [run_blocking_dialog mtype title message ()] opens a dialog box and halts
 * the main GTK loop until some acknowlegement is recieved. The dialog box
 * has the window title [title], GTK message type (determines icon) [mtype],
 * and contains the string content [message]. Takes a unit as its final arg
 * in order to be suitable for use as a GTK callback.
 *)
let run_blocking_dialog mtype title message () =
  let block_dialog = GWindow.message_dialog ~parent:!window_global
      ~message_type:mtype ~resizable:false
      ~title:title
      ~buttons:GWindow.Buttons.ok
      ~message:message () in
  ignore(block_dialog#run ());
  block_dialog#destroy();
  ()

(* EXPOSED SETTER METHODS BEGIN *)

let set_territory_troops name num =
  let button = List.assoc name !buttons_list in
  button#set_label (string_of_int num);
  ()

let rec update_territories (data:(string * string * int) list) =
  (*let temp = List.map (fun x -> match x with | (_, n , _) -> n) data in
  print_endline ("[" ^ (String.concat " , " temp) ^ "]");*)
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
  !player_label_global#set_text player;
  clear_selections();
  ()

let update_available_reinforcements num =
  !reinforcement_label_global#set_text (string_of_int num);
  ()

let update_cards (inf, cav, art, wild) =
  !infantry_label_global#set_text (string_of_int inf);
  !cavalry_label_global#set_text (string_of_int cav);
  !artillery_label_global#set_text (string_of_int art);
  !wildcard_label_global#set_text (string_of_int wild);
  ()

let update_territories_count count =
  !territories_label_global#set_text (string_of_int count);
  ()

let update_troop_count count =
  !troops_label_global#set_text (string_of_int count);
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
    write_log "Game ended. User interface locked.";
    ()
  end
  else ()

let run_blocking_infobox message =
  run_blocking_dialog `QUESTION "Turn Information" message ()

(*
 * This function roll is needed for the Controller to be able to set gui values
 * without creating a situation in which the project cannot compile due to
 * circular dependencies (which occurs when Controller opens Gui).
 *)
let setters = (write_log, update_territories, update_continent_owners,
              update_current_player, update_available_reinforcements,
              update_cards, update_territories_count, update_troop_count,
              set_game_over, run_blocking_infobox)

(* EXPOSED SETTER METHODS END *)

(*
 * [run_init_dialog parent] is a blocking function that creates a dialog window
 * with parent [parent] that allows the user to select the number of players
 * in the game (2 - 6, inclusive). The return value is an int option of None if
 * the user failed to respond (i.e. by closing the window) or Some value of
 * player count if they selected and accepted.
 *)
let run_init_dialog parent =
  let players_num = ref None in
  (*create handler for use later*)
  let init_dialog_accept_handler cbox dialog () =
    let num = ((fst cbox)#active + 2) in
    ignore(dialog#event#send (GdkEvent.create `DELETE));
    players_num :=
      if num = 1 then None
      else Some num;
    dialog#destroy ();
    ()
  in
  (*dialog components*)
  let init_dialog = GWindow.dialog ~parent:parent ~destroy_with_parent:true
                  ~title:"Risc" ~deletable:true
                  ~resizable:false () in
  let init_dialog_label = GMisc.label
                  ~text:"Welcome! Please select the number of players."
                  ~packing:init_dialog#vbox#add () in
  let init_dialog_options = ["2";"3";"4";"5";"6"] in
  let init_dialog_combobox = GEdit.combo_box_text
                  ~strings:init_dialog_options
                  ~packing:init_dialog#vbox#add () in
  (fst init_dialog_combobox)#set_active 0;
  let init_dialog_accept_button = GButton.button ~label:"Accept"
                  ~packing:init_dialog#vbox#add () in
  (*Connect accept button signal*)
  ignore(init_dialog_accept_button#connect#clicked
      ~callback:(init_dialog_accept_handler init_dialog_combobox init_dialog));
  (*connect popup delete signal*)
  ignore(init_dialog#event#connect#delete
      ~callback:(fun _ -> init_dialog#destroy (); true));
  (*Run blocking dialog*)
  ignore(init_dialog#run ());
  !players_num

(*
 * [run_cards_dialog parent] is a blocking function that creates a dialog
 * window with parent [parent] requring them to select a set of 3 cards to play.
 * The return value is an int * int * int option of None if the user failed
 * to respond (i.e. by closing the window) or Some (card1,card2,card3)
 * if they responded correctly, where the int values represent infantry,
 * cavalry, artillery, or wild cards (as values 0-3), respectively.
 *)
let run_cards_dialog parent =
  let cards_selected = ref None in
  let cards_dialog = GWindow.dialog ~parent:parent ~destroy_with_parent:true
                  ~title:"Card Selection Dialog" ~deletable:true
                  ~resizable:false () in
  let cards_dialog_label = GMisc.label
                  ~text:"Please select the cards to exchange."
                  ~packing:cards_dialog#vbox#add () in
  (*Card selection UI components + containers*)
  let cards_frame = GBin.frame ~width:400 ~height:80 ~border_width:3
                  ~packing:cards_dialog#vbox#add () in
  let cards_pack = GPack.hbox ~packing:cards_frame#add () in
  let card1_frame = GBin.frame ~label:"Card 1" ~border_width:2
                  ~packing:cards_pack#add () in
  let card1_cbox = GEdit.combo_box_text ~strings:cards_strings
                  ~packing:card1_frame#add () in
  let card2_frame = GBin.frame ~label:"Card 2" ~border_width:2
                  ~packing:cards_pack#add () in
  let card2_cbox = GEdit.combo_box_text ~strings:cards_strings
                  ~packing:card2_frame#add () in
  let card3_frame = GBin.frame ~label:"Card 3" ~border_width:2
                  ~packing:cards_pack#add () in
  let card3_cbox = GEdit.combo_box_text ~strings:cards_strings
                  ~packing:card3_frame#add () in
  (*Accept button components*)
  let cards_dialog_accept_handler  () =
    (*Get cards*)
    let card1 = (fst card1_cbox)#active in
    let card2 = (fst card2_cbox)#active in
    let card3 = (fst card3_cbox)#active in
    (*save result values*)
    cards_selected :=
      if card1 = -1 || card2 = -1 || card3 = -1 then None
      else Some (card1, card2, card3);
    (*now we can kill the window*)
    let res = cards_dialog#event#send (GdkEvent.create `DELETE) in
    cards_dialog#destroy ();
    ()
  in
  let cards_dialog_accept_button = GButton.button ~label:"Accept"
                  ~packing:cards_dialog#vbox#add () in
  (*Connect accept button signal*)
  ignore(cards_dialog_accept_button#connect#clicked
                  ~callback:(cards_dialog_accept_handler));
  (*connect close event; ensures window is destroyed on x-button*)
  ignore(cards_dialog#event#connect#delete
                  ~callback:(fun _ -> cards_dialog#destroy (); true));
  (*Run blocking dialog loop*)
  ignore(cards_dialog#run ());
  !cards_selected

(*
 * [run_troop_dialog parent message (min, max)] is a blocking function that
 * creates a dialog window with parent [parent] and text content [message]
 * requring them to select some number on a slider between [min] [max]
 * (inclusive). The return value is an int option of None if the user failed
 * to respond (i.e. by closing the window) or Some value if they responded
 * correctly.
 *)
let run_troop_dialog parent message (min, max) =
  let value = ref None in
  let troop_dialog_accept_handler scale dialog () =
    let num = int_of_float scale#adjustment#value in
    value := Some num;
    ignore(dialog#event#send (GdkEvent.create `DELETE));
    dialog#destroy ();
    ()
  in
  let fmin = float_of_int min in
  (*Have to add 10 because for some reason adjustent likes to subtract 10*)
  let fmax = float_of_int (max + 10) in
  let troop_dialog = GWindow.dialog ~parent:parent ~destroy_with_parent:true
                  ~title:"Troop Selection" ~deletable:true
                  ~resizable:false () in
  let troop_dialog_label = GMisc.label
                  ~text:message
                  ~packing:troop_dialog#vbox#add () in
  let troop_dialog_adjustment = GData.adjustment ~value:fmin
                  ~lower:fmin ~upper:fmax ~step_incr:1. (*~page_incr:1.*) () in
  troop_dialog_adjustment#clamp_page ~lower:fmin ~upper:fmax;
  let scale_frame = GBin.frame ~width:200 ~height:50 ~border_width:3
                  ~packing:troop_dialog#vbox#add () in
  let troop_dialog_scale = GRange.scale `HORIZONTAL
                  ~draw_value:true ~digits:0 ~update_policy:`CONTINUOUS
                  ~adjustment:troop_dialog_adjustment
                  ~packing:scale_frame#add () in
  let troop_dialog_accept_button = GButton.button ~label:"Accept"
                  ~packing:troop_dialog#vbox#add () in
  (*Connect selection accept signal to handler*)
  ignore(troop_dialog_accept_button#connect#clicked
      ~callback:(troop_dialog_accept_handler troop_dialog_scale troop_dialog));
  (*Connect close event to ensure window is destroyed on exit*)
  ignore(troop_dialog#event#connect#delete
      ~callback:(fun _ -> troop_dialog#destroy (); true));
  (*Run the dialog in blocking loop*)
  ignore(troop_dialog#run ());
  !value

(*
 * [confirm_button_handler parent] is a function callback for the GUI confirm
 * button with the side effect that it confirms the currently selected action
 * by extracting relevant data from the GUI, creating an Action based on that
 * information, and executing that action on the current state. This alters
 * the value stored by controller.
 *)
let confirm_button_handler parent () =
  begin
  try
    let index = !actions_cbox_global#active in
    let action = (
      (*Deploy: 0*)
      if index = 0 then begin
          let loc = !selection1 in
          match loc with
          | None -> None
          | Some dep -> Some (ADeployment dep)
      end
      (*Attack: 1*)
      else if index = 1 then begin
        (*check that selections were valid and exist*)
        let src = !selection1 in
        let dest = !selection2 in
        match (src, dest) with
        | (Some sloc, Some dloc) -> begin
          (*get troops in source*)
          let src_troops =
            (Controller.get_troops_in_territory !controller sloc)- 1 in
          (*don't bring up slider if we can't select troops*)
          if src_troops = 0 then begin
            write_log ("Unable to comply. "
              ^"Insufficient troops in source territory.");
            None
          end
          (*otherwise show slider*)
          else begin
            let num = run_troop_dialog parent
              "Select the number of troops to attack with." (1, src_troops) in
            (*create action if slider result was valid, abort otherwise*)
            match num with
            | Some n -> write_log ("Attacking " ^ dloc ^ " from "
                                                    ^ sloc ^ " with " ^
                                                    (string_of_int n)
                                                    ^ " unit(s).");
                                          Some (AAttack ((sloc, dloc), n))
            | _ -> None
          end
        end
        (*if we have less than 2 territories selected, abort*)
        | _ ->  write_log "You must have selected two territories to attack.";
                None
      end
      (*Reinforce: 2*)
      else if index = 2 then begin
        (*Check that we have a selection; abort if not*)
        let loc = !selection1 in
          match loc with
          | None -> None
          | Some dep -> begin
            (*get available troops for the player*)
            let aval_troops = (Controller.get_available_reinforcement
                                !controller) in
            (*get desired reinforcement level from user*)
            let num = run_troop_dialog parent
              "Select the number of troops to reinforce with."
              (1, aval_troops) in
            (*proceed only if we get good data*)
            match num with
            | None -> None
            | Some x -> write_log ("Reinforcing " ^ dep ^ " with " ^
                                  (string_of_int x) ^ " troops.");
                        Some (AReinforcement (dep, x))
          end
      end
      (*Move: 3*)
      else if index = 3 then begin
        (*get selections and troop counts*)
        let src = !selection1 in
        let dest = !selection2 in
        (*fetch troops from source region*)
        let src_troops = match src with
        | None -> 0
        | Some loc -> (Controller.get_troops_in_territory !controller loc)- 1 in
        (*don't show dialog box if we have too few troops*)
        if src_troops = 0 then begin
          write_log"Unable to comply. Insufficient troops in source territory.";
          None
        end
        else begin
          let num = run_troop_dialog parent
            "Select the number of troops to move." (1, src_troops) in
          match (src, dest, num) with
          | (Some s, Some d, Some n) -> write_log ("Moving "^(string_of_int n) ^
                                                  " unit(s) from " ^ s ^ " to "
                                                  ^ d ^ ".");
                                        Some (AMovement ((s, d), n))
          | _ -> None
        end
      end
      (*Trade Cards: 4*)
      else if index = 4 then begin
        (*get user's card selections*)
        let cards = run_cards_dialog parent in
        (*only enter logic if they selected cards*)
        match cards with
        | None -> None
        | Some (c_int1, c_int2, c_int3) -> begin
          (*convert cards int ids to variants*)
          let card1 = card_of_int c_int1 in
          let card2 = card_of_int c_int2 in
          let card3 = card_of_int c_int3 in
          (*guarantee that our ids were valid*)
          match (card1, card2, card3) with
          | (Some c1, Some c2, Some c3) ->
                                        (*convert cards to strings for display*)
                                        let st1 = string_of_card c1 in
                                        let st2 = string_of_card c2 in
                                        let st3 = string_of_card c3 in
                                        write_log ("Playing " ^ st1 ^ ", " ^ st2
                                                    ^ ", and " ^ st3 ^ ".");
                                        Some (APlayCards (c1, c2, c3))
          | _ -> None
        end
      end
      (*End Turn: 5*)
      else if index = 5 then begin
        Some ANextTurn
      end
      (*other case is bad data - do nothing*)
      else begin
        None
      end)
    in
    (*Cases over - apply action*)
    match action with
    | None -> ()
    | Some act -> controller := Controller.controller_update
                                  !controller setters act;
                  ();
  with
  | _ ->  write_log "An unexpected error has occurred."
  end;
  (*clear_selections();*)
  ()

(*
 * [actions_cbox_handler box] is a function with the side effect that it
 * sets the gui state based on the action selected in the combobox [box].
 *)
let actions_cbox_handler (box: GEdit.combo_box GEdit.text_combo) () =
  begin
  try
    let index = (fst box)#active in
    (*disallow illegal arguments*)
    if index >= 0 || index >= List.length actions_strings then begin
      let sel = List.nth actions_strings index in
      write_log ("Selected Action: " ^ sel);
      (*Clear and set confirm state after an action is selected*)
      clear_selections ();
      !confirm_button_global#misc#set_sensitive true;
      (*Cases for selection operations*)
      (*Case for deployment*)
      if index = 0 then begin
        write_log ("Select a region by clicking its button.");
        set_selection_mode Single
      end
      (*Case for attacking*)
      else if index = 1 then begin
        write_log ("Click on a source region to attack from, then " ^
                   "a destination region to attack. Then click \"Confirm\".");
        set_selection_mode Double
      end
      (*Case for reinforcement*)
      else if index = 2 then begin
        write_log ("Click on a region to reinforce, then click \"Confirm\".");
        set_selection_mode Single
      end
      (*Case for movement*)
      else if index = 3 then begin
        write_log ("Click on a source region to move troops from, then " ^
                "a destination region to move them to. Then click \"Confirm\"");
        set_selection_mode Double
      end
      (*Otherwise, we don't want to allow selection*)
      else begin
        set_selection_mode No_selection
      end;
    end
    else ();
  with
  | _ ->  write_log "An unexpected error has occurred.";
  end;
  ()

(*
 * [territory_button_handler name button ()] is a function with the side effect
 * that it attempts to select the button specified by [name] and [button],
 * printing a failure message to the log if this is not possible.
 *)
let territory_button_handler name (button: GButton.button) () =
  begin
  try
    write_log ("Region: " ^ name);
    let sel_result = make_selection name in
    (if not sel_result then write_log ("Failed to select " ^ name ^ "."));
    ();
  with
  | _ ->  write_log "An unexpected error has occurred.";
  end;
  ()

(*
 * [cancel_button_handler ()] is a function with the side effect that it clears
 * all territory selections and displayed selection information.
 *)
let cancel_button_handler () =
  begin
    try
      clear_selections ();
    with
    | _ ->  write_log "An unexpected error has occurred.";
  end;
  ()

(*
 * [add_territory pack x y name extra] adds a territory button to the gameplay
 * fixed packing [pack] at pixel coords [x], [y] with tooltip and territory name
 * [name] with private tooltip comment [extra]. Also adds the mapping from
 * [name] to the created button to the territory button list.
 *)
let add_territory (pack:GPack.fixed) x y name extra =
  let button = GButton.button ~label:"0"
                              ~packing:(pack#put ~x:x ~y:y) () in
  button#misc#set_name name;
  GtkData.Tooltips.set_tip (GtkData.Tooltips.create ()) button#as_widget
                          ~text:name ~privat:extra;
  (*Connect the button click signal; throw away value b/c will never change*)
  ignore(button#connect#clicked
                          ~callback: (territory_button_handler name button));
  buttons_list := (name,button)::(!buttons_list);
  ()

(*
 * [add_label pack x y width height name] adds a framed label to the GUI
 * gameplay fixed packing [pack] at pixel coords [x], [y] with a frame
 * of width [width] and height [height] and the name [name]. This function
 * is for use in creating continent labels.
 *)
let add_label (pack:GPack.fixed) x y width height name =
  let label_frame = GBin.frame ~width:width ~height:height
                               ~packing:(pack#put ~x:x ~y:y) () in
  let label = GMisc.label ~text: name
                          ~packing:label_frame#add () in
  continent_labels_list := (name, label_frame)::(!continent_labels_list);
  set_color label_frame "grey";
  ()

(*
 * [main ()] sets requisite mutable global values, prompts the user for
 * appropriate init information, creates the controller, and displays the GUI
 * in its initial state.
 *)
let main () =
  (*Create window and connect close operation*)
  let window = GWindow.window ~width:1450 ~height:860 ~icon:icon_pixbuf
                              ~title:"Risc" ~resizable:false () in
  window_global := window;
  ignore(window#connect#destroy ~callback:Main.quit);

  (*Create packings and labeling frames for widgets to be placed*)
  let top_pane_pack = GPack.paned ~width:1450 ~height:860
                              ~packing:window#add ~border_width:5
                              `HORIZONTAL () in

  let bottom_pane_pack = GPack.paned ~width:1230 ~height:650
                  ~packing:(top_pane_pack#pack1 ~resize:false ~shrink:false)
                  ~border_width:5 `VERTICAL () in

  let sidebar_pack = GPack.paned ~width:220 ~height:850 ~border_width:5
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

  let info_pack = GPack.vbox ~width:210 ~height:340
                  ~packing:info_frame#add
                  () in

  let actions_frame = GBin.frame ~label:"Actions"
                  ~packing:(sidebar_pack#pack2 ~resize:false ~shrink:false)
                  () in

  let actions_pack = GPack.vbox ~width:210 ~height:300 ~spacing:3
                  ~packing:actions_frame#add () in

  (*Get number of players via dialog box; repeat until we get an answer*)
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
  let player_frame = GBin.frame ~label:"Current Player" ~border_width:3
                                ~packing:info_pack#add () in
  let player_label = GMisc.label ~text:"N/A" ~packing:player_frame#add () in
  player_label_global := player_label;

  let reinforcement_frame = GBin.frame ~label:"Reinforcements Available"
                                ~border_width:3 ~packing:info_pack#add () in
  let reinforcement_label = GMisc.label ~text:"0"
                                        ~packing:reinforcement_frame#add () in
  reinforcement_label_global := reinforcement_label;

  let territories_frame = GBin.frame ~label:"Territories Controlled"
                                    ~border_width:3 ~packing:info_pack#add () in
  let territories_label = GMisc.label ~text:"0"
                                      ~packing:territories_frame#add () in
  territories_label_global := territories_label;

  let troops_frame = GBin.frame ~label:"Troops In Play"
                                    ~border_width:3 ~packing:info_pack#add () in
  let troops_label = GMisc.label ~text:"0"
                                 ~packing:troops_frame#add () in
  troops_label_global := troops_label;

  (*Cards in inforpack *)
  let cards_frame = GBin.frame  ~label:"Cards" ~border_width:3
                                ~packing:info_pack#add () in
  let cards_pack = GPack.vbox ~spacing:2 ~border_width:2
                              ~packing:cards_frame#add () in

  let infantry_frame = GBin.frame ~label:"Infantry" ~border_width:3
                                  ~packing:cards_pack#add () in
  let infantry_label = GMisc.label ~text:"0" ~packing:infantry_frame#add () in
  infantry_label_global := infantry_label;

  let cavalry_frame = GBin.frame  ~label:"Cavalry" ~border_width:3
                                  ~packing:cards_pack#add () in
  let cavalry_label = GMisc.label ~text:"0" ~packing:cavalry_frame#add () in
  cavalry_label_global := cavalry_label;

  let artillery_frame = GBin.frame  ~label:"Artillery" ~border_width:3
                                    ~packing:cards_pack#add () in
  let artillery_label = GMisc.label ~text:"0" ~packing:artillery_frame#add () in
  artillery_label_global := artillery_label;

  let wildcard_frame = GBin.frame ~label:"Wildcards" ~border_width:3
                                  ~packing:cards_pack#add () in
  let wildcard_label = GMisc.label ~text:"0" ~packing:wildcard_frame#add () in
  wildcard_label_global := wildcard_label;

  (*Action pack setup*)
  let actions_cbox_frame = GBin.frame ~label:"Move Selection" ~border_width:3
                  ~packing:actions_pack#add () in
  let actions_cbox = GEdit.combo_box_text
              ~strings:actions_strings
              ~packing:actions_cbox_frame#add () in
  ignore((fst actions_cbox)#connect#changed
                        ~callback:(actions_cbox_handler actions_cbox));
  actions_cbox_global := fst actions_cbox;

  let confirm_button = GButton.button ~label:"Confirm"
                                      ~packing:actions_pack#add () in
  ignore(confirm_button#connect#clicked
                              ~callback:(confirm_button_handler window));
  confirm_button_global := confirm_button;

  let cancel_button = GButton.button ~label:"Cancel"
                                      ~packing:actions_pack#add () in
  ignore(cancel_button#connect#clicked ~callback:cancel_button_handler);

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
    ("> Game started with " ^ (string_of_int !player_num) ^ " players.\n" ^
     "> Select a move from \"Move Selection\" in the right pane.");

  (*Menu bar creation*)
  let menubar = GMenu.menu_bar ~packing:(info_pack#add) () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let help_menu = factory#add_submenu "Help" in

  (*File menu setup*)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore(factory#add_item "Quit" ~callback: Main.quit);

  (*Help menu setup*)
  let factory = new GMenu.factory help_menu ~accel_group in
  ignore(factory#add_item "About" ~callback:(run_blocking_dialog `INFO "About"
    ("Risc is a OCaml implementation of the classic strategy game\nRisk, and a"^
    " final project for our Fall 2017 CS3110 class.\n\nDeveloped by:\n\t- "^
    "Avani Bhargava (ab2387@cornell.edu)\n\t- Haram Kim (hk592@cornell.edu)"^
    "\n\t- Samuel Ringel (sjr254@cornell.edu)\n\t- Rachel Shim "^
    "(cs899@cornell.edu)\n\nYou can find further documentation at:"^
    "\nhttps://github.com/rachelshim/Risc/blob/master/README.md")));
  ignore(factory#add_item "Instructions" ~callback:(run_blocking_dialog `INFO 
  "Instructions" ("The game is divided into a setup and gameplay phase.\n"^
    "In the setup phase, players take turns DEPLOY-ing 1 troop at a time.\n"^
    "When all players have finished setup, we enter the gameplay phase.\n"^
    "During this section, players are required to begin their turns by\n"^
    "using the REINFORCE command to reinforce territories. They may then\n"^
    "choose to SPEND CARDS, if available. After this, players may ATTACK\n"^
    "any number of times. Once finished, players may either MOVE once,\n"^
    "shifting troops across controlled contiguous territory, or END TURN.\n"^
    "Please be aware that if you elect not to MOVE, you must END TURN twice\n"^
    "so that your cards may be allocated.")));

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

  (*Set some sensitivities before game starts*)
  set_territory_buttons_sensitivity false;
  confirm_button#misc#set_sensitive false;

  (*Final window configuration and display*)
  window#add_accel_group accel_group;
  window#set_icon (Some icon_pixbuf);
  window#show ();
  let pmap = GdkPixbuf.create_pixmap map_pixbuf |> fst in
  Gdk.Window.set_back_pixmap gameplay_pack#misc#window (`PIXMAP pmap);

  (*Initialize game; must go here to preserve button look and feel*)
  controller := Controller.init_game !player_num setters;

  (*main GTK loop*)
  Main.main ()

(* Run GTK loop*)
let () = main ()
