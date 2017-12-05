gui:
	corebuild -use-ocamlfind gui.byte

play:
	(corebuild -use-ocamlfind gui.byte) > play_build.log && ./gui.byte

controller:
	corebuild -use-ocamlfind controller.byte

state:
	corebuild -use-ocamlfind state.byte

clean:
	corebuild -clean