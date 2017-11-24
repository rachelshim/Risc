open GMain
open GdkKeysyms
open Gtk
open Unix
open Thread

let locale = GtkMain.Main.init ()

let map_pixbuf = GdkPixbuf.from_file "resources/map.png"

let mymutex = Core.Mutex.create ()

let myfunction () =
  Mutex.lock mymutex;
  (*Unix.sleep 5;*)
  prerr_endline "lock/unlock";
  Mutex.unlock mymutex

let main () =
  let window = GWindow.window ~width:1500 ~height:800
                                    ~title:"Risc" () in
    let a = window#connect#destroy ~callback:Main.quit; in
    let pane_pack = GPack.paned `HORIZONTAL ~width:1500 ~height:800 () in
    let gameplay_pack = GPack.fixed ~has_window:true ~width:1230 ~height:650 ~packing:window#add () in
    
      (* Menu bar *)
      let menubar = GMenu.menu_bar ~packing:(gameplay_pack#add) () in
        let factory = new GMenu.factory menubar in
        let accel_group = factory#accel_group in
        let file_menu = factory#add_submenu "File" in

        (* File menu *)
        let factory = new GMenu.factory file_menu ~accel_group in
        let b = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in
      
          (* Button *)
          let button = GButton.button ~label:"Push me!"
                                      ~packing:(gameplay_pack#put ~x:0 ~y:0)
                                      (*~packing:(table#attach ~left:0 ~top:0)*)
                                      (*~packing:(bbox#add)*) () in
          let c = button#connect#clicked ~callback: (myfunction) in
          
          (*disable button example*)
          (*button#misc#set_sensitive false;*)  

              (* Display the windows and enter Gtk+ main loop *)
              window#add_accel_group accel_group;
              window#show ();

              let pmap = GdkPixbuf.create_pixmap map_pixbuf |> fst in
              Gdk.Window.set_back_pixmap gameplay_pack#misc#window (`PIXMAP pmap);

              Main.main ()

let () = main ()