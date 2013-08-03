

(**
  @raise Not_found
 **)
let whereis ?(paths=[]) ?(path_suffixes=[""]) file =
  let fullpaths = List.fold_right (fun p fullpaths ->
      List.fold_right (fun suf fullpaths -> 
          (p ^ "/" ^ suf) :: fullpaths
        ) path_suffixes fullpaths
    ) paths [] 
  in
  List.find (fun p -> 
      Sys.file_exists (p ^ "/" ^ file)
    ) fullpaths


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
    let path = whereis ~paths ~path_suffixes filename in
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
      let path = whereis ~paths ~path_suffixes filename in
      if Mac.(!policy <> PreferFramework)
      then Framework (path, name)
      else Library (path ^ "/" ^ filename)          
    )
    else
      raise Not_found


