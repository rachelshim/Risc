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

test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

debug:
	ocamlbuild -use-ocamlfind gui.d.byte

gitlog:
	git log --stat > ../gitlog.txt

zip:
	zip -r --exclude=*.git* ../Risc_archive.zip .