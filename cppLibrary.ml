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
    match lib with
      | Library filename -> ( 
	match soname filename with
	  | Some name -> A ("-l" ^ name)
	  | None -> 
          if to_c then A filename
          else S []
      )
      | Framework (path, name) -> S [A ("-F" ^ path) ; A "-framework" ; A name]
  in

  List.iter (fun library ->
    flag [ "ocamlmklib" ; "link" ; "c" ; "use_"^name] (link_one true library) ;
    flag [ "ocamlmklib" ; "link" ; "ocaml" ; "use_"^name] (link_one false library)
  ) libraries
