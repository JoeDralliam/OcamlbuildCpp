open OcamlbuildCppConfiguration
open Ocamlbuild_plugin
open Pathname

let add_cpp_rules cppcompiler =
  let compiler_name = Compiler.name cppcompiler in
  let compile = A (Compiler.BuildFlags.command cppcompiler) in
  let obj = Compiler.object_extension cppcompiler in
  let os_name = OS.(name current) in


  rule "c++ : cpp -> (o|obj)"
    ~dep:"%.cpp"
    ~prod:("%." ^ obj) (fun env builder ->
        let cpp = env "%.cpp" in
        let obj = env ("%." ^ obj) in
        let tags = tags_of_pathname cpp ++ "compile" ++ "c++" ++ compiler_name ++ os_name in

        let nolink = Compiler.BuildFlags.nolink cppcompiler in
        let output = Compiler.BuildFlags.output_obj cppcompiler obj in

        Cmd (S [ compile ; A nolink ; A cpp ; T tags ; A output ] )
      ) 

let add_cpp_common_tags () =
  flag [ "compile" ; "c++" ; "gcc" ] (A "-fPIC") ;
  flag [ "compile" ; "c++" ; "gcc" ; "release" ] (A "-O3") ;
  flag [ "compile" ; "c++" ; "gcc" ; "debug" ] (A "-g") ;

  flag [ "compile" ; "c++" ; "clang" ] (A "-fPIC") ;
  flag [ "compile" ; "c++" ; "clang" ; "release" ] (A "-O3") ;
  flag [ "compile" ; "c++" ; "clang" ; "debug" ] (A "-g") ;

  flag [ "compile" ; "c++" ; "msvc" ] 
    (S [A "/nologo" ; A "/MD" ; A "/EHs"]) ;
  flag [ "compile" ; "c++" ; "msvc" ; "release" ] (A "/Ox") ; 

  flag [ "link" ; "shared" ; "linux" ] (A "-shared") ;
  flag [ "link" ; "shared" ; "mac" ] (A "-shared") ;

  flag [ "link" ; "shared" ; "mac" ; "clang" ]
    (S [A "-flat_namespace" ; A "-undefined" ; A "suppress"]) ;

  flag [ "link" ; "shared" ; "windows" ; "msvc" ]
    (S [A "-chain" ; A "msvc" ; A "-merge-manifest" ])
