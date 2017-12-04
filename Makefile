gui:
	corebuild -use-ocamlfind gui.byte

play:
	corebuild -use-ocamlfind gui.byte && ./gui.byte

controller:
	corebuild -use-ocamlfind controller.byte

state:
	corebuild -use-ocamlfind state.byte