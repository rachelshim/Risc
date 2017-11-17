open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

let mymutex = Core.Mutex.create ()

