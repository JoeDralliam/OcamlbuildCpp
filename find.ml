exception IncludeDirectoryNotFound of string

module type LibraryConfigurationType =
sig
  type component
  type human_version
  type version

  val name : string

  val root_env_variables : string list
  val includedir_env_variables : string list
    
  val includedir_search_paths_from_root : string -> string list
  val includedir_search_paths_system : string list
  val includedir_search_paths_suffixes : string list
  val includedir_file : string

  val library_search_dirs_from_root : string -> string list
  val library_search_dirs_from_includedir : string -> string list
  val library_search_dirs_system : version -> string list
  val library_search_dirs_suffixes : string list

  val library_names : static:bool -> cppcompiler:CppCompiler.t -> component -> version -> string list
  val extract_version : string -> version
  val as_human_version : version -> human_version
end

module Make = 
  functor (LibraryConfiguration: LibraryConfigurationType) ->
struct
  module ComponentsOrder =
  struct
    type t = LibraryConfiguration.component
    let compare = Pervasives.compare
  end
  
  module LibraryMap = Map.Make (ComponentsOrder)

  type version = LibraryConfiguration.human_version
  type t =
    {
      includedir: string ;
      library: CppCompiler.Library.t LibraryMap.t ;
      version: version
    }





  let get_root ?root () =
    let root = ref root in
    List.iter (fun env ->
      if !root = None && (Env.get_with_default env "") <> ""
      then root := Some (Env.get env)
    ) LibraryConfiguration.root_env_variables ;
    !root

  let get_includedir_search_paths ?root () =
    let from_root = 
      match root with
      | Some rt -> LibraryConfiguration.includedir_search_paths_from_root rt
      | None -> []
    in
    let system = LibraryConfiguration.includedir_search_paths_system in
    from_root @ system

  let get_includedir ?root ?includedir () =
    let includedir = ref includedir in
    List.iter (fun env ->
      if !includedir = None && (Env.get_with_default env "") <> ""
      then includedir := Some (Env.get env)
    ) LibraryConfiguration.includedir_env_variables ;    
    match !includedir with
    | Some dir -> dir
    | None ->
      let paths = get_includedir_search_paths ?root () in
      let path_suffixes = LibraryConfiguration.includedir_search_paths_suffixes in
      try
        File.whereis ~paths ~path_suffixes LibraryConfiguration.includedir_file
      with
      | Not_found -> raise (IncludeDirectoryNotFound LibraryConfiguration.name)

  let get_library_search_dirs ?root includedir version = 
    let from_root = 
      match root with
      | Some rt -> LibraryConfiguration.library_search_dirs_from_root rt
      | None -> []
    in
    let from_includedir =
      LibraryConfiguration.library_search_dirs_from_includedir includedir
    in
    let system =
      LibraryConfiguration.library_search_dirs_system version
    in
    from_root @ from_includedir @ system

  let try_add_component ~static ~cppcompiler library_search_dirs libraries version component =
    let names = LibraryConfiguration.library_names ~static ~cppcompiler component version in
    let path_suffixes = LibraryConfiguration.library_search_dirs_suffixes in
    try 
      let lib = File.find_library_n ~paths:library_search_dirs ~path_suffixes ~static names cppcompiler in
      libraries := LibraryMap.add component lib !libraries
    with 
      Not_found -> ()

  let find ?root ?includedir ?(static=false) cppcompiler components =
    let root = get_root ?root () in
    let includedir = get_includedir ?includedir ?root () in
    let version = LibraryConfiguration.extract_version includedir in
    let library_search_dirs = get_library_search_dirs ?root includedir version in
    let libraries = ref LibraryMap.empty in
    List.iter (try_add_component ~static ~cppcompiler library_search_dirs libraries version) components ;
    { library = !libraries ; includedir ; version = LibraryConfiguration.as_human_version version }
end
