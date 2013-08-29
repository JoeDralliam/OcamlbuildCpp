




(**
   may_retry f args: apply f to args until success **)
let rec may_retry f = function
  | [] -> None
  | arg :: t ->
    match f arg with
      | None -> may_retry f t
      | Some res -> Some (res, arg)


let rec may_retry_opt f = function
  | [] -> None
  | None :: t -> may_retry_opt f t
  | Some arg :: t ->
    match f arg with
      | None -> may_retry_opt f t
      | Some res -> Some (res, arg)


let whereis fullpaths file =
  try
    Some (List.find (fun p -> Sys.file_exists (p ^ "/" ^ file)) fullpaths)
  with
    | Not_found -> None



let find_file ?(paths=[]) ?(path_suffixes=[""]) file = 
  let fullpaths = List.fold_right (fun p fullpaths ->
      List.fold_right (fun suf fullpaths -> 
          let fullpath = 
            if suf <> "" 
            then (p ^ "/" ^ suf)
            else p
          in fullpath :: fullpaths
        ) path_suffixes fullpaths
    ) paths [] 
  in


  let framework_header file =
    let reg = Str.regexp "\\([A-Za-z][A-Za-z0-9_]*\\)/\\([A-Za-z][A-Za-z0-9_]*\\.hpp\\)" in
    if Str.string_match reg file 0
    then Some (Str.matched_group 1 file ^ ".framework/Headers/" ^ Str.matched_group 2 file)
    else None
  in
  
  let files_to_look =
    if Conf.OS.(current = Mac)
    then 
      let open CppCompiler.Library.Mac in
      match !policy with 
	| PreferFramework -> [ framework_header file ; Some file ]
	| PreferLibrary -> [ Some file ; framework_header file ]
    else [ Some file ]
  in

  match may_retry_opt (whereis fullpaths) files_to_look with
    | Some (path, filename) -> path, filename
    | None -> raise Not_found


let find_library_impl fullpaths ~static cppcompiler name =
  let open CppCompiler.Library in
  let library_filename name =
    if static
    then  static_library_filename name cppcompiler
    else dynamic_library_filename name cppcompiler
  in

  let files_to_look  =
    if Conf.OS.(current = Mac)
    then 
      match Mac.(!policy) with 
	| Mac.PreferFramework -> [ framework_filename name cppcompiler ; library_filename name]
	| Mac.PreferLibrary   -> [ library_filename name ; framework_filename name cppcompiler]
    else [ library_filename name ]
  in
  match may_retry (whereis fullpaths) files_to_look with
    | Some (path, filename) ->
      if Ocamlbuild_plugin.Pathname.check_extension filename "framework"
      then Some (Framework (path, name))
      else Some (Library (path ^ "/" ^ filename))
    | None -> None



let find_library ?(paths=[]) ?(path_suffixes=[""]) ~static cppcompiler names =
  let fullpaths = List.fold_right (fun p fullpaths ->
      List.fold_right (fun suf fullpaths -> 
          let fullpath = 
            if suf <> "" 
            then (p ^ "/" ^ suf)
            else p
          in fullpath :: fullpaths
        ) path_suffixes fullpaths
    ) paths [] 
  in

  match may_retry (find_library_impl fullpaths ~static cppcompiler) names with
    | Some (lib, _) -> lib
    | None -> raise Not_found


let for_each_line filename f =
  let chan = open_in filename in
  try
    while true do
      f (input_line chan)
    done ;
    assert false
  with 
    | End_of_file -> 
      close_in chan
