Risc
Avani Bhargava (ab2387), Haram Kim (hk592), Sam Ringel (sjr254), Rachel Shim (cs899)

Compilation Directions:
-Prerequisites:
    -GTK+ 2.x: A GTK environment must be present on the compiling machine.
        GTK can be installed via Apt through the package "libgtk2.0-dev",
        or via Homebrew for OSX. Windows is theoretically supported
        but has not been tested, and GTK implementations on Windows suffer
        from significant feature limitations.
    -lablgtk: The local OCaml environment must have lablgtk installed.
        This can be achieved by running 'opam install lablgtk'. 

The Makefile provided in the source code archive provides several compilation 
options, many of which are irrelevant for running the game normally. To compile
Risc for normal use, without starting the program, use 'make gui'. To compile 
and run the program, use 'make play'.

In the event that the compiling machine does not have GNU Make, the following 
command can be used to execute ocamlbuild: 
    ocamlbuild -use-ocamlfind gui.byte
The program may then be run manually by executing gui.byte.