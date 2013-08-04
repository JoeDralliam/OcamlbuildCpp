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
      flag ["link"; "library"; "ocaml"; "byte"]
        (S[A"-cclib"; A"conf_stubs.o"]);
      
      flag ["link"; "library"; "ocaml"; "native"]
        (S[A"-cclib"; A"conf_stubs.o"]);
      
      dep  ["link"; "ocaml"; "library"] 
        ["conf_stubs.o"]
    | _ -> ()
  end
