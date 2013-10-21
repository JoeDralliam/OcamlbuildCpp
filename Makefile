all:native byte

byte:
	ocamlbuild -use-ocamlfind ocamlbuildcpp.cma

native:
	ocamlbuild -use-ocamlfind ocamlbuildcpp.cmxa

clean:
	ocamlbuild -clean

install: uninstall
	ocamlfind install ocamlbuildcpp META _build/ocamlbuildcpp.cm[a,xa] \
					_build/ocamlbuildCppConfiguration.cm[i,x,o] \
					_build/ocamlbuildCpp.cm[i,x,o] \
					_build/ocamlbuildcpp.* \
					_build/libconf_stubs.* \
					_build/dllconf_stubs.* \
					_build/Configuration/oS.cm[x,o] \
					_build/Configuration/env.cm[i,x,o] \
					_build/Cpp/compiler.cm[x,o] \
					_build/Cpp/file.cm[x,o] \
					_build/Cpp/findLibrary.cm[x,o] \
					_build/Cpp/findSfml.cm[x,o] \
					_build/Cpp/findBoost.cm[x,o] \
					_build/Cpp/rule.cm[x,o] \
					_build/Cpp/library.cm[x,o]

uninstall:
	ocamlfind remove ocamlbuildcpp

.PHONY:install uninstall
