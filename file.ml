
let framework_header_file file =
  let reg = Str.regexp "\\([A-Za-z][A-Za-z0-9_]*\\)/\\([A-Za-z][A-Za-z0-9_]*\\.hpp\\)" in
  if Str.string_match reg file 0
  then Some (Str.matched_group 1 file ^ ".framework/Headers/" ^ Str.matched_group 2 file)
  else None


(**
  @raise Not_found
 **)
let whereis_impl ?(paths=[]) ?(path_suffixes=[""]) file =
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
  List.find (fun p -> 
      Sys.file_exists (p ^ "/" ^ file)
    ) fullpaths

let whereis ?(paths=[]) ?(path_suffixes=[""]) file =
  let (file,otherwise) =
     if Conf.OS.(current = Mac)
     then match framework_header_file file with
          | Some fk -> (fk,Some file)
          | None -> (file,None)
      else (file,None)
   in
   try (whereis_impl ~paths ~path_suffixes file, file)
   with Not_found ->
       match otherwise with
       | Some file -> (whereis_impl ~paths ~path_suffixes file, file)
       | None -> raise Not_found 

let find_library ?(paths=[]) ?(path_suffixes=[""]) ~static name cppcompiler =
  let open CppCompiler.Library in
  try
    let filename =
      if Mac.(!policy = PreferFramework)
      then framework_filename name cppcompiler
      else if static
      then  static_library_filename name cppcompiler
      else dynamic_library_filename name cppcompiler
    in
    let path = whereis_impl ~paths ~path_suffixes filename in
    if Mac.(!policy = PreferFramework)
    then Framework (path, name)
    else Library (path ^ "/" ^ filename)
  with Not_found ->
    if Conf.OS.current == Conf.OS.Mac
    then (
      let filename = 
        if Mac.(!policy <> PreferFramework)
        then framework_filename name cppcompiler
        else if static
        then  static_library_filename name cppcompiler
        else dynamic_library_filename name cppcompiler
      in
      let path = whereis_impl ~paths ~path_suffixes filename in
      if Mac.(!policy <> PreferFramework)
      then Framework (path, name)
      else Library (path ^ "/" ^ filename)          
    )
    else
      raise Not_found


let rec find_library_n ?(paths=[]) ?(path_suffixes=[""]) ~static names cppcompiler =
  match names with
  | [] -> raise Not_found
  | name :: names ->
    try 
      find_library ~paths ~path_suffixes ~static name cppcompiler
    with Not_found ->
      find_library_n ~paths ~path_suffixes ~static names cppcompiler

