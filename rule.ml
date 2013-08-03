open Ocamlbuild_plugin
open Pathname


let add_cpp_rules cppcompiler =
  let compiler_name = CppCompiler.name cppcompiler in
  let compile = A (CppCompiler.BuildFlags.command cppcompiler) in
  let obj = CppCompiler.object_extension cppcompiler in
  let os_name = Conf.OS.(name current) in

  let parallel dir files = List.map (fun f -> [dir/f]) files in


  rule "c++ : cpp -> (o|obj)"
    ~dep:"%.cpp"
    ~prod:("%." ^ obj) (fun env builder ->
        let cpp = env "%.cpp" in
        let obj = env ("%." ^ obj) in
        let tags = tags_of_pathname cpp ++ "compile" ++ "c++" ++ compiler_name ++ os_name in

        let nolink = CppCompiler.BuildFlags.nolink cppcompiler in
        let output = CppCompiler.BuildFlags.output_obj cppcompiler obj in

        Cmd (S [ compile ; A nolink ; A cpp ; T tags ; A output ] )
      ) ;

  if CppCompiler.(frontend cppcompiler = GccCompatible)
  then 
    rule "c++ : cpplib -> a"
      ~dep:"%(path)/%(libname).cpplib"
      ~prod:"%(path)/lib%(libname).a" (fun env builder ->
          let cpplib = env "%(path)/%(libname).cpplib" in
          let archive = env "%(path)/lib%(libname).a" in
          let tags = tags_of_pathname cpplib ++ "link" ++ "static" ++ "c++" ++ compiler_name ++ os_name in

          let dir = dirname cpplib in
          let o_files = string_list_of_file cpplib in
          List.iter Outcome.ignore_good (builder (parallel dir o_files));

          let obtain_spec_obj o = A (dir/o) in
          let spec_obj_list =(List.map obtain_spec_obj o_files) in
	  Cmd(S[A "ar" ; A "-q" ; Px archive ; T tags; S spec_obj_list ])
        )
  else 
    rule "c++ : cpplib -> lib"
      ~dep:"%(path)/%(libname).cpplib"
      ~prod:"%(path)/lib%(libname).lib" (fun env builder ->
          let cpplib = env "%(path)/%(libname).cpplib" in
          let archive = env "%(path)/lib%(libname).lib" in
          let tags = tags_of_pathname cpplib ++ "link" ++ "static" ++ "c++" ++ compiler_name ++ os_name in

          let dir = dirname cpplib in
          let o_files = string_list_of_file cpplib in
          List.iter Outcome.ignore_good (builder (parallel dir o_files));

          let obtain_spec_obj o = A (dir/o) in
          let spec_obj_list =(List.map obtain_spec_obj o_files) in
	  Cmd(S[A "link.exe" ; A "/NOLOGO" ; A ("/OUT:" ^ archive) ; T tags; S spec_obj_list ])
        ) ;

  if Conf.OS.(current = Windows)
  then 
    rule "c++ : cpplib -> dll"
      ~dep:"%(path)/%(libname).cpplib"
      ~prod:"%(path)/dll%(libname).dll" (fun env builder ->
          let link = A "flexlink" in
          let cpplib = env "%(path)/%(libname).cpplib" in
          let dynamiclib = env "%(path)/dll%(libname).dll" in
          let tags = tags_of_pathname cpplib ++ "link" ++ "shared" ++ "c++" ++  compiler_name ++ os_name in
          let o_files = string_list_of_file cpplib in
          let dir = dirname cpplib in
          List.iter Outcome.ignore_good (builder (parallel dir o_files));
          let obtain_spec_obj o = A (dir/o) in
          let spec_obj_list = (List.map obtain_spec_obj o_files) in
          Cmd( S[ link ; S spec_obj_list ; T tags ; A "-o" ; Px dynamiclib] )
        )
  else 
    rule "c++ : cpplib -> so"
      ~dep:"%(path)/%(libname).cpplib"
      ~prod:"%(path)/dll%(libname).so" (fun env builder ->
          let link = compile in
          let cpplib = env "%(path)/%(libname).cpplib" in
          let dynamiclib = env "%(path)/dll%(libname).so" in
          let tags = tags_of_pathname cpplib ++ "link" ++ "shared" ++ "c++" ++  compiler_name ++ os_name in
          let o_files = string_list_of_file cpplib in
          let dir = dirname cpplib in
          List.iter Outcome.ignore_good (builder (parallel dir o_files));
          let obtain_spec_obj o = A (dir/o) in
          let spec_obj_list = (List.map obtain_spec_obj o_files) in
          Cmd (S [ link ; S spec_obj_list ; T tags ; A "-o" ; Px dynamiclib] )
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


  flag [ "link" ; "shared" ; "linux" ] (A "-shared") ;
  flag [ "link" ; "shared" ; "mac" ] (A "-shared") ;

  flag [ "link" ; "shared" ; "mac" ; "clang" ]
    (S [A "-flat_namespace" ; A "-undefined" ; A "suppress"]) ;

  flag [ "link" ; "shared" ; "windows" ; "msvc" ]
    (S [A "-chain" ; A "msvc" ; A "-merge-manifest" ])
