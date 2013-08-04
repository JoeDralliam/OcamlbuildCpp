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
		let archive =
			if Sys.os_type = "Win32"
			then ("libconf_stubs.lib")
			else ("libconf_stubs.a")
		in
		flag ["link"; "library"; "ocaml"; "byte"]
			(S[A"-cclib"; A"-lconf_stubs"]);
      
		flag ["link"; "library"; "ocaml"; "native"]
			(S[A"-cclib"; A"-lconf_stubs"]);
      
		dep ["link"; "ocaml"; "library"]
			[archive]
    | _ -> ()
  end
