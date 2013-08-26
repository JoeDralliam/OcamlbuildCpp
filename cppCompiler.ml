






type t = Gcc | Clang | MSVC | MinGW | Cygwin

type front = GccCompatible | MSVCCompatible           

external default_compiler : unit -> t = "conf_compiler"

let maybe_available =
  (default_compiler ()) ::
    match Conf.OS.current with
    | Conf.OS.Linux -> [ Gcc ; Clang ]
    | Conf.OS.Mac -> [ Clang ; Gcc ]
    | Conf.OS.Windows -> [ MSVC ; MinGW ; Cygwin ]
                         
let is_available comp =
  let cmd = match comp with
    | Gcc -> "g++ --version"
    | Clang -> "clang++ --version"
    | MSVC -> "cl.exe /HELP"
    | MinGW -> "g++ --version"
    | Cygwin -> "g++ --version"
  in
  (Sys.command cmd) = 0

let available () =
  List.filter is_available maybe_available

let frontend = function
  | Gcc | Clang | MinGW | Cygwin -> GccCompatible
  | MSVC -> MSVCCompatible

let name = function
    | Gcc -> "gcc"
    | Clang -> "clang"
    | MSVC -> "msvc"
    | MinGW -> "mingw"
    | Cygwin -> "cygwin"

let object_extension comp = 
  match frontend comp with
  | GccCompatible -> "o"
  | MSVCCompatible -> "obj"

module BuildFlags =
struct

  let command = function
    | Gcc -> "g++"
    | Clang -> "clang++"
    | MSVC -> "cl.exe"
    | MinGW -> "g++"
    | Cygwin -> "g++"


  let release comp =
    match frontend comp with
    | GccCompatible -> "-O3"
    | MSVCCompatible -> "/Ox"

  let debug comp =
    match frontend comp with
    | GccCompatible -> "-g"
    | MSVCCompatible -> "/Zi"

  let output_obj comp output =
    match frontend comp with
    | GccCompatible -> "-o" ^ output 
    | MSVCCompatible -> "/Fo" ^ output

  let nolink comp =
    match frontend comp with
    | GccCompatible -> "-c"
    | MSVCCompatible -> "/c"

  let add_include_path path comp =
    match frontend comp with
    | GccCompatible -> "-I"^path
    | MSVCCompatible -> "/I"^path
end

module Library =
struct
  type t =
      Library of string (* filename (with path) or -lname *)
    | Framework of string * string (* path, library name *)


  module Mac =
  struct
    type policy = PreferLibrary | PreferFramework
                  
    let policy = 
      if Conf.OS.current = Conf.OS.Mac
      then ref PreferFramework
      else ref PreferLibrary

    let prefer_framework () =
      policy := PreferFramework

    let prefer_library () =
      policy := PreferLibrary
  end

  let framework_filename name comp =
    name ^ ".framework"

  let static_library_filename name comp =
    match frontend comp with
      | GccCompatible -> "lib" ^ name ^ ".a"
      | MSVCCompatible -> name ^ ".lib"

  let dynamic_library_filename name comp =
    match Conf.OS.current with
      | Conf.OS.Linux -> "lib" ^ name ^ ".so"
      | Conf.OS.Mac -> "lib" ^ name ^ ".dylib" 
      | Conf.OS.Windows when comp = MSVC -> name ^ ".lib"
      | Conf.OS.Windows -> "lib" ^ name ^ ".a"

  let caml_static_extension comp = 
    match frontend comp with
      | GccCompatible -> "a"
      | MSVCCompatible -> "lib"

  let caml_dynamic_extension comp = 
    match Conf.OS.current with
      | Conf.OS.Linux -> "so"
      | Conf.OS.Mac -> "so" 
      | Conf.OS.Windows -> "s.obj"

  let soname filename =
    let regexp = Str.regexp ".*lib\\([^/]+\\)\\.so" in
    if Str.string_match regexp filename 0
    then Some (Str.matched_group 1 filename)
    else None 
end

module Version =
struct
  type t = { major: int ; minor : int }

  external msvc_major : unit -> int = "conf_msvc_version_major"
  external msvc_minor : unit -> int = "conf_msvc_version_minor"

  let detect comp =
    if comp = MSVC
    then { major = msvc_major () ; minor = msvc_minor () }
    else 
      let dumpcom = (BuildFlags.command comp) ^ " -dumpversion" in
      let version_str = input_line (Unix.open_process_in dumpcom) in
      let regexp = Str.regexp "\\([0-9]\\)\\.\\([0-9]\\)(\\.[0-9])?" in
      if Str.string_match regexp version_str 0
      then { major = int_of_string (Str.matched_group 1 version_str) ;
             minor = int_of_string (Str.matched_group 2 version_str) }
      else failwith "Could not detect compiler version"

  let dumpversion comp =
    if comp = MSVC
    then string_of_int (msvc_major () * 100 + msvc_minor ())
    else 
      let dumpcom = (BuildFlags.command comp) ^ " -dumpversion" in
      let version_str = input_line (Unix.open_process_in dumpcom) in
      let regexp = Str.regexp "\\([0-9]\\)\\.\\([0-9]\\)(\\.[0-9])?" in
      if Str.string_match regexp version_str 0
      then (Str.matched_group 1 version_str) ^ (Str.matched_group 2 version_str)
      else ""
    
end

