all:native byte

byte:
	ocamlbuild -use-ocamlfind ocamlbuildcpp.cma

native:
	ocamlbuild -use-ocamlfind ocamlbuildcpp.cmxa

clean:
	ocamlbuild -clean

install: uninstall
	ocamlfind install ocamlbuildcpp META _build/ocamlbuildcpp.cm[a,xa] \
					_build/ocamlbuildcpp.* \
					_build/conf_stubs.o \
					_build/conf.cm[i,x,o] \
					_build/cppCompiler.cm[i,x,o] \
					_build/file.cm[i,x,o] \
					_build/findSfml.cm[i,x,o] \
					_build/rule.cm[i,x,o]
uninstall:
	ocamlfind remove ocamlbuildcpp

.PHONY:install uninstall