open Ocamlbuild_plugin
open Command


let _ = 
  dispatch begin function
    | Before_rules ->
      rule "%.obj -> %.o" ~dep:"%.obj" ~prod:"%.o" (fun env builder ->
          let obj = env "%.obj" in
          let o = env "%.o" in
	  mv obj o
	)
    | After_rules ->
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
