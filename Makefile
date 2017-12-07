gui:
	ocamlbuild -use-ocamlfind gui.byte

play:
	(ocamlbuild -use-ocamlfind gui.byte) > play_build.log && ./gui.byte

controller:
	ocamlbuild -use-ocamlfind controller.byte

state:
	ocamlbuild -use-ocamlfind state.byte

clean:
	ocamlbuild -clean