type components =
  System | Window | Graphics | Audio | Network | Main

let string_of_component = function
  | System -> "system"
  | Window -> "window"
  | Graphics -> "graphics"
  | Audio -> "audio"
  | Network -> "network"
  | Main -> "main"


type version = 
  {
    major:int ;
    minor:int 
  }


type sfml_libraries =
  {
    system:CppCompiler.Library.t ;
    window:CppCompiler.Library.t ;
    graphics:CppCompiler.Library.t ;
    network:CppCompiler.Library.t ;
    audio:CppCompiler.Library.t ;
    main:CppCompiler.Library.t
  }

let invalid_sfml_libraries = 
  let open CppCompiler.Library in
  { system = Library "" ; window = Library "" ; graphics = Library "" ; audio = Library "" ; network = Library "" ; main = Library "" }

type sfml =
  {
    default: sfml_libraries ;
    release:sfml_libraries option ;
    debug:sfml_libraries option ;
    include_dir:string ;
    version: version
  }

let add_component libs component lib =
  match component with
  | System   -> { libs with system   = lib }
  | Window   -> { libs with window   = lib }
  | Graphics -> { libs with graphics = lib }
  | Audio    -> { libs with audio    = lib }
  | Network  -> { libs with network  = lib }
  | Main     -> { libs with main     = lib }

let extract_version sfml_include_dir =
  let config_hpp = sfml_include_dir ^ "/SFML/Config.hpp" in
  let chan = open_in config_hpp in


  (* If not found, < 2.0 and minor is unknown *)
  let major = ref 1 in
  let minor = ref (-1) in
  try
    while true do
      let line = input_line chan in

      let maj_reg = Str.regexp "#define SFML_VERSION_MAJOR \\([0-9]+\\)" in
      if Str.string_match maj_reg line 0
      then (
        let maj_str = Str.matched_group 1 line in
        major := int_of_string maj_str
      ) ;

      let min_reg = Str.regexp "#define SFML_VERSION_MINOR \\([0-9]+\\)" in
      if Str.string_match min_reg line 0
      then (
        let min_str = Str.matched_group 1 line in
        minor := int_of_string min_str
      )
    done ;
    assert false
  with End_of_file -> 
    { 
      major = !major ; 
      minor = !minor
    }


(**
  @raise Not_found
 **)
let find ?(static=false) cppcompiler components =
  let suffix = if static then "-s" else "" in
  
  let paths =
    ["~/Library/Frameworks" ; "/Library/Frameworks" ; 
     "/usr/local" ; "/usr" ; "/sw" ;
     "/opt/local" ; "/opt/csw" ; "/opt"] @
        try
          [Sys.getenv "SFMLDIR"]
        with 
          Not_found -> []
  in
  let sfml_include_dir = 
    File.whereis ~path_suffixes:["include"] ~paths "SFML/Config.hpp"
  in
  let version = extract_version sfml_include_dir in  


  let libs = List.map (fun component ->
      let component_string = string_of_component component in
      let component_name = 
        if component = Main
        then "sfml-main"
        else "sfml-"^component_string^suffix
      in
      let release =
        try Some (File.find_library ~paths ~path_suffixes:["lib" ; "lib64"] 
                    ~static component_name cppcompiler)
        with Not_found -> None
      in
      let debug =
        try Some (File.find_library ~paths ~path_suffixes:["lib" ; "lib64"]
                    ~static (component_name ^ "-d") cppcompiler)
        with Not_found -> None
      in
      let default =
        match (release,debug) with
        | (Some r, _) -> r
        | (None, Some d) -> d
        | (None, None) -> raise Not_found
      in
      (component, default, release, debug) 
    ) components in
  let release =
    let rec extract = function
      | [] -> []
      | (comp, _, Some r, _)::t -> (comp, r)::(extract t) 
      | (_, _, None, _) :: _ -> []
    in
    let extracted = extract libs in
    if extracted = []
    then None
    else
      let res = ref invalid_sfml_libraries in
      List.iter (fun (c,l) -> res := add_component !res c l) extracted ;
      Some !res
  in
  let debug =
    let rec extract = function
      | [] -> []
      | (comp, _, _, Some d)::t -> (comp, d)::(extract t) 
      | (_, _, _, None) :: _ -> []
    in
    let extracted = extract libs in
    if extracted = []
    then None
    else
      let res = ref invalid_sfml_libraries in
      List.iter (fun (c,l) -> res := add_component !res c l) extracted ;
      Some !res
  in
  let default =
    let rec extract = function
      | [] -> []
      | (comp, d, _, _)::t -> (comp, d)::(extract t) 
    in
    let extracted = extract libs in
    let res = ref invalid_sfml_libraries in
    List.iter (fun (c,l) -> res := add_component !res c l) extracted ;
    !res
  in

  {
    default ; debug ; release ; version ;
    include_dir = sfml_include_dir
  }
        
