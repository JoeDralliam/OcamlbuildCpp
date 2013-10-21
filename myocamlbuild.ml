open Ocamlbuild_plugin
open Command


let _ = 
  dispatch begin function
    | After_rules ->
      Pathname.define_context "Cpp" [ "Configuration" ; "Cpp" ] ;

      let archive = "libconf_stubs."^ !Options.ext_lib in
      let dynalib = "dllconf_stubs."^ !Options.ext_dll in


      flag ["link"; "library"; "ocaml"]
        (S[A"-cclib"; A"-lconf_stubs"]);


      flag ["link"; "library"; "ocaml"; "byte"]
        (S[A"-dllib"; A"-lconf_stubs"]);


      dep ["link"; "ocaml"; "library"]
        [archive ; dynalib]
    | _ -> ()
  end
