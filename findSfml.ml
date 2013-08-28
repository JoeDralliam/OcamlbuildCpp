module SfmlConfiguration =
struct
  type component =
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
  type human_version = version  

  let extract_version config_hpp =
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

  let name = "SFML"

  let root_env_variables = ["SFMLDIR"]
  let includedir_env_variables = []

  let includedir_search_paths_from_root rt = [rt]
  let includedir_search_paths_system =
    ["~/Library/Frameworks" ; "/Library/Frameworks" ; 
     "/usr/local" ; "/usr" ; "/sw" ;
     "/opt/local" ; "/opt/csw" ; "/opt"]
  let includedir_search_paths_suffixes = ["" ; "include"]
  let includedir_file = "SFML/Config.hpp"

  let library_search_dirs_from_root rt = [rt]
  let library_search_dirs_from_includedir _ = [] 
  let library_search_dirs_system _ = includedir_search_paths_system
  let library_search_dirs_suffixes = [ "" ; "lib" ; "lib64" ]
  let library_names ~static ~cppcompiler component _ =
    let suffix = if static then "-s" else "" in  
    let component_string = string_of_component component in
    if component = Main
    then ["sfml-main"]
    else ["sfml-"^component_string^suffix]

  let as_human_version version = version
end

module Sfml = Find.Make(SfmlConfiguration)

