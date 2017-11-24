open GMain
open GdkKeysyms
open Gtk
open Unix
open Thread

let locale = GtkMain.Main.init ()

let map_pixbuf = GdkPixbuf.from_file "resources/map.png"

let mymutex = Core.Mutex.create ()

let myfunction x =
  Mutex.lock mymutex;
  (*Unix.sleep 5;*)
  prerr_endline "lock/unlock";
  Mutex.unlock mymutex;
  true

let main () =
  let window = GWindow.window ~width:1600 ~height:650
                              ~title:"Risc" ~resizable:false () in
    let window_exit_signal = window#connect#destroy ~callback:Main.quit; in
    let pane_pack = GPack.paned ~width:1600 ~height:650 
                                ~packing:window#add ~border_width:5 
                                `HORIZONTAL () in

    let gameplay_pack = GPack.fixed ~has_window:true ~width:1227 ~height:650 
                    ~packing:(pane_pack#pack1 ~resize:false ~shrink:false) () in

    let sidebar_pack = GPack.paned ~width:370 ~height:650 ~border_width:5 
                    ~packing:(pane_pack#pack2 ~resize:false ~shrink:false)
                    `VERTICAL () in

    let info_pack = GPack.vbox ~width:360 ~height:240 
                    ~packing:(sidebar_pack#pack1 ~resize:false ~shrink:false)
                    () in

    let cards_pack = GPack.vbox ~width:360 ~height:400 
                    ~packing:(sidebar_pack#pack2 ~resize:false ~shrink:false)
                    () in

      (* Menu bar *)
      let menubar = GMenu.menu_bar ~packing:(gameplay_pack#add) () in
        let factory = new GMenu.factory menubar in
        let accel_group = factory#accel_group in
        let file_menu = factory#add_submenu "File" in

        (* File menu *)
        let factory = new GMenu.factory file_menu ~accel_group in
        let file_quit_signal = factory#add_item "Quit" ~key:_Q 
                                      ~callback: Main.quit in
      
          (* Button *)
          let button = GButton.button ~label:"0"
                        ~packing:(gameplay_pack#put ~x:68 ~y:60) () in
          let a = GtkData.Tooltips.set_tip (GtkData.Tooltips.create ()) button#as_widget ~text:"Alaska" ~privat:"Part of North America" in
          let button_signal = button#event#connect#button_press ~callback: (myfunction) in
          

            (* Display the windows and enter Gtk+ main loop *)
            window#add_accel_group accel_group;
            window#show ();

            let pmap = GdkPixbuf.create_pixmap map_pixbuf |> fst in
            Gdk.Window.set_back_pixmap gameplay_pack#misc#window (`PIXMAP pmap);

            Main.main ()

let () = main ()