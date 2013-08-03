 open Ocamlbuild_plugin
 open Command
 
let _ = 
  dispatch begin function
    | After_rules ->
      ocaml_lib "ocamlbuildcpp";
      
      flag ["link"; "library"; "ocaml"; "byte"; "use_libocamlbuildcpp"]
        (S[A"-dllib"; A"-locamlbuildcpp"]);
      
      flag ["link"; "library"; "ocaml"; "native"; "use_libocamlbuildcpp"]
        (S[A"-cclib"; A"-locamlbuildcpp"]);
      
      dep  ["link"; "ocaml"; "use_libocamlbuildcpp"] 
        ["dllocamlbuildcpp.so" ; "libocamlbuildcpp.a"]
    | _ -> ()
  end
