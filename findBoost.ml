open CppCompiler

module BoostConfiguration =
struct
  type component =
      System | Filesystem | DateTime | Iostreams | Regex | Thread | ProgramOptions | Signals

  let component_name = function
    | System -> "system"
    | Filesystem -> "filesystem"
    | DateTime -> "date_time" 
    | Iostreams -> "iostreams"
    | Regex -> "regex"
    | Thread -> "thread"
    | ProgramOptions -> "program_options"
    | Signals -> "signals"



  type human_version =
    { major : int ; minor : int ; patch : int }
  type version = human_version * string

  let string_of_version v =
    string_of_int v.major ^ "_" ^ string_of_int v.minor

  let string_of_version_with_patch v =
    string_of_int v.major ^ "_" ^ string_of_int v.minor ^ "_" ^ string_of_int v.patch


  let guess_compiler_prefix comp boost_version =
    match comp with
    | MSVC -> begin
        match (Version.msvc_major (), Version.msvc_minor ()) with
        | (11, _) -> "-vc110"
        | (10, _) -> "-vc100"
        | ( 9, _) -> "-vc90"
        | ( 8, _) -> "-vc80"
        | ( 7,10) -> "-vc71"
        | ( 7, _) -> "-vc70"
        | ( 6, _) -> "-vc60"
        | _ -> failwith "FindBoost.guess_compiler_prefix: Unsupported version of MSVC"
      end
    | MinGW when (boost_version.major * 100 + boost_version.minor) < 134 -> "-mgw"
    | MinGW -> "-mgw" ^ (Version.dumpversion comp)
    | Gcc when (boost_version.major * 100 + boost_version.minor) < 134 -> "-gcc"
    | Gcc ->
      if Conf.OS.(current = Mac)
      then 
        if boost_version.minor > 35
        then "-xgcc" ^ (Version.dumpversion comp)
        else ""
      else "-gcc" ^ (Version.dumpversion comp)
    | Clang when (boost_version.major * 100 + boost_version.minor) < 134 -> "-gcc"
    | Clang ->
      if Conf.OS.(current = Mac)
      then 
        if boost_version.minor > 35
        then "-clang-darwin" ^ (Version.dumpversion comp)
        else ""
      else "-gcc" ^ (Version.dumpversion comp)
    | _ -> ""


  let known_versions = 
    let rec range from to_ =
      if from == to_ - 1
      then []
      else { major = 1 ; minor = from ; patch = 0} :: range (from - 1) to_
    in range 56 33




  let extract_version boost_includedir =
    let version_hpp = boost_includedir ^ "/boost/version.hpp" in
    let chan = open_in version_hpp in


    let version = ref 0 in
    let lib_version = ref "" in
    try
      while true do
        let line = input_line chan in

        let version_reg = Str.regexp "#define BOOST_VERSION \\([0-9]+\\).*" in
        if Str.string_match version_reg line 0
        then (
          let version_str = Str.matched_group 1 line in
          version := int_of_string version_str
        ) ;

        let lib_version_reg = Str.regexp "#define BOOST_LIB_VERSION \"\\([0-9_]+\\)\".*" in
        if Str.string_match lib_version_reg line 0
        then (
          lib_version := Str.matched_group 1 line
        )
      done ;
      assert false
    with End_of_file -> 
      ( { major = !version / 100_000 ; minor = (!version / 100) mod 1_000 ; patch = !version mod 100 } , 
        !lib_version)


  let name = "boost"
  let multithreaded = ref false

  let root_env_variables = ["Boost_DIR" ; "BOOST_ROOT" ; "BOOSTROOT"]
  let includedir_env_variables = ["BOOST_INCLUDEDIR"]

  let includedir_search_paths_from_root rt = [rt ^ "/include" ; rt]
  let includedir_search_paths_system = 
    [ "C:/boost/include" ; "C:/boost" ] @
      (Env.do_with_default "ProgramFiles" (fun pf -> [pf ^ "/boost/include" ; pf ^ "/boost"]) []) @
      ["/usr/local/include" ; "/usr/include" ; "/sw/local/include"]
  let includedir_search_paths_suffixes =
    let p = List.map (fun v ->
        [ "boost-" ^ string_of_version v ;
          "boost-" ^ string_of_version_with_patch v ;
          "boost_" ^ string_of_version v ;
          "boost_" ^ string_of_version_with_patch v ]
      ) known_versions
    in 
    "" :: (List.flatten p)
  let includedir_file = "boost/version.hpp"

  let library_search_dirs_from_root rt = [ rt ^ "/lib" ; rt ^ "/stage/lib" ]
  let library_search_dirs_from_includedir includedir =
    [ includedir ^ "/lib" ; includedir ^ "/../lib" ; includedir ^ "/stage/lib"]
  let library_search_dirs_system (version,_) =
    [ "C:/boost/lib" ; "C:/boost"] @
      (Env.do_with_default "ProgramFiles"
         (fun pf -> [ pf ^ "/boost/boost_" ^ (string_of_version_with_patch version) ^ "/lib" ;
                      pf ^ "/boost/boost_" ^ (string_of_version version) ^ "/lib" ;
                      pf ^ "/boost/lib" ; pf ^ "/boost"]) []) @
      ["/sw/local/lib" ; "/usr/local/lib" ; "/usr/lib" ]
  let library_search_dirs_suffixes = [""]

  let library_names ~static ~cppcompiler component (version, lib_version) =
    let lib_prefix =
      if Conf.OS.(current = Windows) && static && cppcompiler <> Cygwin
      then "lib"
      else ""
    in

    let compiler = guess_compiler_prefix cppcompiler version in
    let multithreaded_suffix =
      if !multithreaded
      then "-mt"
      else ""
    in
    let compname = component_name component in
    [Printf.sprintf "%sboost_%s%s%s-%s" lib_prefix compname compiler multithreaded_suffix lib_version ;
     Printf.sprintf "%sboost_%s%s%s" lib_prefix compname compiler multithreaded_suffix ;
     Printf.sprintf "%sboost_%s%s-%s" lib_prefix compname multithreaded_suffix lib_version ;
     Printf.sprintf "%sboost_%s%s" lib_prefix compname multithreaded_suffix ;
     Printf.sprintf "%sboost_%s" lib_prefix compname]

  let as_human_version = fst 
end

module Boost = Find.Make(BoostConfiguration)

(* 
open CppCompiler

type components =
  System | Filesystem | DateTime | Iostreams | Regex | Thread | ProgramOptions | Signals

let component_name = function
  | System -> "system"
  | Filesystem -> "filesystem"
  | DateTime -> "date_time" 
  | Iostreams -> "iostreams"
  | Regex -> "regex"
  | Thread -> "thread"
  | ProgramOptions -> "program_options"
  | Signals -> "signals"

module ComponentsOrder =
struct
  type t = components
  let compare = Pervasives.compare
end

module LibraryMap = Map.Make (ComponentsOrder)


type version =
  { major : int ; minor : int }

type library =
  {
    libraries: Library.t LibraryMap.t ;
    version: version ;
    includedir: string
  }


let string_of_version v =
  string_of_int v.major ^ "_" ^ string_of_int v.minor

let string_of_version_with_patch v =
  string_of_int v.major ^ "_" ^ string_of_int v.minor ^ "_0"


let guess_compiler_prefix comp boost_version =
  match comp with
  | MSVC -> begin
      match (Version.msvc_major (), Version.msvc_minor ()) with
      | (11, _) -> "-vc110"
      | (10, _) -> "-vc100"
      | ( 9, _) -> "-vc90"
      | ( 8, _) -> "-vc80"
      | ( 7,10) -> "-vc71"
      | ( 7, _) -> "-vc70"
      | ( 6, _) -> "-vc60"
      | _ -> failwith "FindBoost.guess_compiler_prefix: Unsupported version of MSVC"
    end
  | MinGW when (boost_version.major * 100 + boost_version.minor) < 134 -> "-mgw"
  | MinGW -> "-mgw" ^ (Version.dumpversion comp)
  | Gcc when (boost_version.major * 100 + boost_version.minor) < 134 -> "-gcc"
  | Gcc ->
     if Conf.OS.(current = Mac)
     then 
       if boost_version.minor > 35
       then "-xgcc" ^ (Version.dumpversion comp)
       else ""
     else "-gcc" ^ (Version.dumpversion comp)
  | Clang when (boost_version.major * 100 + boost_version.minor) < 134 -> "-gcc"
  | Clang ->
     if Conf.OS.(current = Mac)
     then 
       if boost_version.minor > 35
       then "-clang-darwin" ^ (Version.dumpversion comp)
       else ""
     else "-gcc" ^ (Version.dumpversion comp)
  | _ -> ""


let known_versions = 
  let rec range from to_ =
    if from == to_
    then [ { major = 1 ; minor = from} ]
    else { major = 1 ; minor = from} :: range (from - 1) to_
  in range 56 33




let extract_version boost_includedir =
  let version_hpp = boost_includedir ^ "/boost/version.hpp" in
  let chan = open_in version_hpp in


  let version = ref 0 in
  let lib_version = ref "" in
  try
    while true do
      let line = input_line chan in

      let version_reg = Str.regexp "#define BOOST_VERSION \\([0-9]+\\).*" in
      if Str.string_match version_reg line 0
      then (
        let version_str = Str.matched_group 1 line in
        version := int_of_string version_str
      ) ;

      let lib_version_reg = Str.regexp "#define BOOST_LIB_VERSION \"\\([0-9_]+\\)\".*" in
      if Str.string_match lib_version_reg line 0
      then (
        lib_version := Str.matched_group 1 line
      )
    done ;
    assert false
  with End_of_file -> 
    ( { major = !version / 100_000 ; minor = (!version / 100) mod 1_000 } , !lib_version)




let find 
    ?boost_version ?(exact_boost_version=false) ?(static=false) ?(multithreaded=false) comp components =
  let test_versions =
    if exact_boost_version
    then 
      match boost_version with
      | Some v -> [v]
      | None -> failwith "Required an exact boost version but boost version is not specified"
    else 
      match boost_version with
      | Some v ->
        List.filter (fun kv -> kv.major * 100 + kv.minor >= v.major * 100 + v.minor) known_versions
      | None -> 
        known_versions
  in
  let include_search_dirs_system =
    [ "C:/boost/include" ; "C:/boost" ] @
      (Env.do_with_default "ProgramFiles" (fun pf -> [pf ^ "/boost/include" ; pf ^ "/boost"]) []) @
      ["/usr/local/include" ; "/usr/include" ; "/sw/local/include"]
  in
  let boost_root = ref None in
  let boost_includedir = ref None in

  if !boost_root = None && (Env.get_default "Boost_DIR" "") <> ""
  then boost_root := Some (Env.get "Boost_DIR") ;

  if !boost_root = None && (Env.get_default "BOOST_ROOT" "") <> ""
  then boost_root := Some (Env.get "BOOST_ROOT") ;

  if !boost_root = None && (Env.get_default "BOOSTROOT" "") <> ""
  then boost_root := Some (Env.get "BOOSTROOT") ;

  if (Env.get_default "BOOST_INCLUDEDIR" "") <> ""
  then boost_includedir := Some (Env.get "BOOST_INCLUDEDIR") ;

  let include_search_dirs =
    match !boost_root with
    | Some rt ->
      [rt ^ "/include" ; rt] @ include_search_dirs_system
    | None -> 
      include_search_dirs_system
  in



  let boost_includedir =
    match !boost_includedir with
    | Some dir -> dir
    | None ->
        let path_suffixes =
          let p = List.map (fun v ->
              let majstr = string_of_int v.major in
              let minstr = string_of_int v.minor in
              [ "boost-" ^ majstr ^ "_" ^ minstr ;
                "boost-" ^ majstr ^ "_" ^ minstr ^ "_0" ;
                "boost_" ^ majstr ^ "_" ^ minstr ;
                "boost_" ^ majstr ^ "_" ^ minstr ^ "_0" ]
            ) test_versions
          in "" :: (List.flatten p)
        in
        File.whereis ~paths:include_search_dirs ~path_suffixes "boost/config.hpp"
  in

  let (version,lib_version) = extract_version boost_includedir in
  Printf.printf "Found boost version %s" lib_version ;
  let lib_prefix =
    if Conf.OS.(current = Windows) && static && comp <> Cygwin
    then "lib"
    else ""
  in

  let compiler = guess_compiler_prefix comp version in
  let multithreaded_suffix =
    if multithreaded
    then "-mt"
    else ""
  in

  let library_search_dirs =
    match !boost_root with
    | Some rt -> [ rt ^ "/lib" ; rt ^ "/stage/lib" ]
    | None -> []
  in
  let library_search_dirs =
    library_search_dirs @
      [ boost_includedir ^ "/lib" ; boost_includedir ^ "/../lib" ; boost_includedir ^ "/stage/lib"]
  in
  let library_search_dirs_system =
    [ "C:/boost/lib" ; "C:/boost"] @
      (Env.do_with_default "ProgramFiles"
         (fun pf -> [ pf ^ "/boost/boost_" ^ (string_of_version_with_patch version) ^ "/lib" ;
                      pf ^ "/boost/boost_" ^ (string_of_version version) ^ "/lib" ;
                      pf ^ "/boost/lib" ; pf ^ "/boost"]) []) @
      ["/sw/local/lib"]
  in
  let library_search_dirs =
    library_search_dirs @ library_search_dirs_system
  in



  let libs = ref LibraryMap.empty in
  List.iter (fun component ->
      let compname = component_name component in
      let release_names = 
         [Printf.sprintf "%sboost_%s%s%s-%s" lib_prefix compname compiler multithreaded_suffix lib_version ;
          Printf.sprintf "%sboost_%s%s%s" lib_prefix compname compiler multithreaded_suffix ;
          Printf.sprintf "%sboost_%s%s-%s" lib_prefix compname multithreaded_suffix lib_version ;
          Printf.sprintf "%sboost_%s%s" lib_prefix compname multithreaded_suffix ;
          Printf.sprintf "%sboost_%s" lib_prefix compname]
      in
      try 
        let lib = File.find_library_n ~paths:library_search_dirs ~static release_names comp in
        libs := LibraryMap.add component lib !libs 
      with 
        Not_found -> ()
    ) components ;
  { libraries = !libs ; version ; includedir = boost_includedir }
*)
