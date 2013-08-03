all:native byte

byte:
	ocamlbuild -use-ocamlfind ocamlbuildcpp.cma

native:
	ocamlbuild -use-ocamlfind ocamlbuildcpp.cmxa

clean:
	ocamlbuild -clean
