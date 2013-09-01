open Ocamlbuild_plugin

let register ?(libraries=[]) ?includedir name compiler =
  begin match includedir with
    | Some dir -> 
      let add_includedir = A (CppCompiler.BuildFlags.add_include_path dir compiler) in
      flag [ "compile" ; "c"   ; "use_"^name ] & S [A "-ccopt" ; add_includedir] ;
      flag [ "compile" ; "c++" ; "use_"^name ] add_includedir
    | None -> ()
  end ;

  let link_one to_c lib =
    let open CppCompiler.Library in
    match lib.kind with
      | Library -> 
	if to_c
	then A (lib.path ^ "/" ^ lib.identifier)
	else S []
      | Shared    -> S [A ("-L" ^ lib.path) ; A ("-l" ^ lib.identifier) ]
      | Framework -> S [A ("-F" ^ lib.path) ; A "-framework" ; A lib.identifier]
  in

  List.iter (fun library ->
    flag [ "ocamlmklib" ; "c" ; "use_"^name] (link_one true library) ;
    flag [ "ocamlmklib" ; "ocaml" ; "use_"^name] (link_one false library)
  ) libraries

