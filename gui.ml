open GMain
open GdkKeysyms
open Unix
open Thread

let locale = GtkMain.Main.init ()

let mymutex = Core.Mutex.create ()

let myfunction () =
  Mutex.lock mymutex;
  Unix.sleep 5;
  prerr_endline "lock/unlock";
  Mutex.unlock mymutex

let rec heartbeat () = 
  print_endline "beat";
  Unix.sleep 1;
  heartbeat ();
  ()

let main () =
  let window = GWindow.window ~width:320 ~height:240
                                    ~title:"Simple lablgtk program" () in
    let vbox = GPack.vbox ~packing:window#add () in
    let a = window#connect#destroy ~callback:Main.quit in

      (* Menu bar *)
      let menubar = GMenu.menu_bar ~packing:vbox#pack () in
        let factory = new GMenu.factory menubar in
        let accel_group = factory#accel_group in
        let file_menu = factory#add_submenu "File" in

        (* File menu *)
        let factory = new GMenu.factory file_menu ~accel_group in
        let b = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

          (* Button *)
          let button = GButton.button ~label:"Push me!"
                                            ~packing:vbox#add () in
            let c = button#connect#clicked ~callback: (myfunction) in

              (* Display the windows and enter Gtk+ main loop *)
              window#add_accel_group accel_group;
              window#show ();
              Main.main ()

let thr = Thread.create heartbeat ()
let () = main ()