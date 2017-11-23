open GMain
open GdkKeysyms
open Gtk
open GtkBase
open GtkMain
open Unix
open Thread
open GnomeCanvas
open GnoCanvas

let locale = GtkMain.Main.init ()

let map_pixbuf = GdkPixbuf.from_file "resources/map.png"

let mymutex = Core.Mutex.create ()

let myfunction () =
  Mutex.lock mymutex;
  (*Unix.sleep 5;*)
  prerr_endline "lock/unlock";
  Mutex.unlock mymutex

let main () =
  let window = GWindow.window ~width:1400 ~height:700
                                    ~title:"Risc" () in
    let a = window#connect#destroy ~callback:Main.quit; in
    let vbox = GPack.vbox ~packing:window#add () in
    


      (* Menu bar *)
      let menubar = GMenu.menu_bar ~packing:vbox#pack () in
        let factory = new GMenu.factory menubar in
        let accel_group = factory#accel_group in
        let file_menu = factory#add_submenu "File" in

        (* File menu *)
        let factory = new GMenu.factory file_menu ~accel_group in
        let b = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

          (* Button *)
          (*)
          let button = GButton.button ~label:"Push me!"
                                            ~packing:vbox#add () in
          let c = button#connect#clicked ~callback: (myfunction) in
          *)
          let canvas = GnoCanvas.canvas                               
                                        ~packing:vbox#add () in
          let canvas_background = GnoCanvas.pixbuf ~x:0. ~y:50. ~pixbuf:map_pixbuf ~props:[ `ANCHOR `CENTER ] canvas#root in  

            (*disable button example*)
            (*button#misc#set_sensitive false;*)  

              (* Display the windows and enter Gtk+ main loop *)
              window#add_accel_group accel_group;
              window#show ();
              Main.main ()

let () = main ()