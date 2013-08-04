module OcamlbuildSignatures =
struct
  module type OrderedTypePrintable =
    sig
      type t
      val compare :
        t -> t -> int
      val print :
        Format.formatter -> t -> unit
    end
  module type SET =
    sig
      type elt
      type t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> t * bool * t
      val find : (elt -> bool) -> t -> elt
      val map : (elt -> elt) -> t -> t
      val of_list : elt list -> t
      val print : Format.formatter -> t -> unit
    end
  module type LIST =
    sig
      val print :
        (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a list -> unit
      val filter_opt : ('a -> 'b option) -> 'a list -> 'b list
      val union : 'a list -> 'a list -> 'a list
      val length : 'a list -> int
      val hd : 'a list -> 'a
      val tl : 'a list -> 'a list
      val nth : 'a list -> int -> 'a
      val rev : 'a list -> 'a list
      val append : 'a list -> 'a list -> 'a list
      val rev_append : 'a list -> 'a list -> 'a list
      val concat : 'a list list -> 'a list
      val flatten : 'a list list -> 'a list
      val iter : ('a -> unit) -> 'a list -> unit
      val map : ('a -> 'b) -> 'a list -> 'b list
      val rev_map : ('a -> 'b) -> 'a list -> 'b list
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
      val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
      val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
      val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
      val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
      val fold_left2 :
        ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
      val fold_right2 :
        ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
      val for_all : ('a -> bool) -> 'a list -> bool
      val exists : ('a -> bool) -> 'a list -> bool
      val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val mem : 'a -> 'a list -> bool
      val memq : 'a -> 'a list -> bool
      val find : ('a -> bool) -> 'a list -> 'a
      val filter : ('a -> bool) -> 'a list -> 'a list
      val find_all : ('a -> bool) -> 'a list -> 'a list
      val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
      val assoc : 'a -> ('a * 'b) list -> 'b
      val assq : 'a -> ('a * 'b) list -> 'b
      val mem_assoc : 'a -> ('a * 'b) list -> bool
      val mem_assq : 'a -> ('a * 'b) list -> bool
      val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
      val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
      val split : ('a * 'b) list -> 'a list * 'b list
      val combine : 'a list -> 'b list -> ('a * 'b) list
      val sort : ('a -> 'a -> int) -> 'a list -> 'a list
      val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
      val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
      val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
    end
  module type STRING =
    sig
      val print : Format.formatter -> string -> unit
      val chomp : string -> string
      val before : string -> int -> string
      val after : string -> int -> string
      val first_chars : string -> int -> string
      val last_chars : string -> int -> string
      val eq_sub_strings : string -> int -> string -> int -> int -> bool
      val is_prefix : string -> string -> bool
      val is_suffix : string -> string -> bool
      val contains_string : string -> int -> string -> int option
      val subst : string -> string -> string -> string
      val tr : char -> char -> string -> string
      val rev : string -> string
      val implode : char list -> string
      val explode : string -> char list
      external length : string -> int = "%string_length"
      external get : string -> int -> char = "%string_safe_get"
      external set : string -> int -> char -> unit = "%string_safe_set"
      external create : int -> string = "caml_create_string"
      val make : int -> char -> string
      val copy : string -> string
      val sub : string -> int -> int -> string
      val fill : string -> int -> int -> char -> unit
      val blit : string -> int -> string -> int -> int -> unit
      val concat : string -> string list -> string
      val iter : (char -> unit) -> string -> unit
      val escaped : string -> string
      val index : string -> char -> int
      val rindex : string -> char -> int
      val index_from : string -> int -> char -> int
      val rindex_from : string -> int -> char -> int
      val contains : string -> char -> bool
      val contains_from : string -> int -> char -> bool
      val rcontains_from : string -> int -> char -> bool
      val uppercase : string -> string
      val lowercase : string -> string
      val capitalize : string -> string
      val uncapitalize : string -> string
      type t = string
      val compare : t -> t -> int
      external unsafe_get : string -> int -> char = "%string_unsafe_get"
      external unsafe_set : string -> int -> char -> unit
        = "%string_unsafe_set"
      external unsafe_blit : string -> int -> string -> int -> int -> unit
        = "caml_blit_string" "noalloc"
      external unsafe_fill : string -> int -> int -> char -> unit
        = "caml_fill_string" "noalloc"
    end
  module type TAGS =
    sig
      type elt = string
      type t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> t * bool * t
      val of_list : string list -> t
      val print : Format.formatter -> t -> unit
      val does_match : t -> t -> bool
      module Operators :
        sig
          val ( ++ ) : t -> elt -> t
          val ( -- ) : t -> elt -> t
          val ( +++ ) : t -> elt option -> t
          val ( --- ) : t -> elt option -> t
        end
    end
  module type PATHNAME =
    sig
      type t = string
      val concat : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val exists : t -> bool
      val mk : string -> t
      val define_context : string -> string list -> unit
      val include_dirs_of : string -> string list
      val copy : t -> t -> unit
      val to_string : t -> string
      val print : Format.formatter -> t -> unit
      val current_dir_name : t
      val parent_dir_name : t
      val read : t -> string
      val same_contents :
        t -> t -> bool
      val basename : t -> t
      val dirname : t -> t
      val is_relative : t -> bool
      val readlink : t -> t
      val readdir : t -> t array
      val is_link : t -> bool
      val is_directory : t -> bool
      val add_extension :
        string -> t -> t
      val check_extension : t -> string -> bool
      val get_extension : t -> string
      val remove_extension : t -> t
      val update_extension :
        string -> t -> t
      val get_extensions : t -> string
      val remove_extensions : t -> t
      val update_extensions :
        string -> t -> t
      val print_path_list :
        Format.formatter -> t list -> unit
      val pwd : t
      val parent : t -> t
      val is_prefix : t -> t -> bool
      val is_implicit : t -> bool
      module Operators :
        sig
          val ( / ) :
            t ->
            t -> t
          val ( -.- ) :
            t -> string -> t
        end
    end
  module type COMMAND =
    sig
      type tags
      type pathname
      type t =
          Seq of t list
        | Cmd of spec
        | Echo of string list * pathname
        | Nop
      and spec =
          N
        | S of spec list
        | A of string
        | P of pathname
        | Px of pathname
        | Sh of string
        | T of tags
        | V of string
        | Quote of spec
      val atomize : string list -> spec
      val atomize_paths : string list -> spec
      val execute :
        ?quiet:bool -> ?pretend:bool -> t -> unit
      val execute_many :
        ?quiet:bool ->
        ?pretend:bool ->
        t list -> (bool list * exn) option
      val setup_virtual_command_solver :
        string -> (unit -> spec) -> unit
      val search_in_path : string -> string
      val reduce : spec -> spec
      val print : Format.formatter -> t -> unit
      val to_string : t -> string
      val string_of_command_spec : spec -> string
    end
  module type GLOB =
    sig
      type globber
      val parse : ?dir:string -> string -> globber
      exception Parse_error of string
      val eval : globber -> string -> bool
    end
  module type LOG =
    sig
      val level : int Pervasives.ref
      val dprintf :
        int -> ('a, Format.formatter, unit) Pervasives.format -> 'a
      val eprintf : ('a, Format.formatter, unit) Pervasives.format -> 'a
      val raw_dprintf :
        int -> ('a, Format.formatter, unit) Pervasives.format -> 'a
    end
  module type OUTCOME =
    sig
      type ('a, 'b) t = Good of 'a | Bad of 'b
      val wrap : ('a -> 'b) -> 'a -> ('b, exn) t
      val ignore_good : ('a, exn) t -> unit
      val good : ('a, exn) t -> 'a
    end
  module type MISC =
    sig
      val opt_print :
        (Format.formatter -> 'a -> unit) ->
        Format.formatter -> 'a option -> unit
      val the : 'a option -> 'a
      val getenv : ?default:string -> string -> string
      val with_input_file :
        ?bin:bool -> string -> (Pervasives.in_channel -> 'a) -> 'a
      val with_output_file :
        ?bin:bool -> string -> (Pervasives.out_channel -> 'a) -> 'a
      val with_temp_file : string -> string -> (string -> 'a) -> 'a
      val read_file : string -> string
      val copy_chan : Pervasives.in_channel -> Pervasives.out_channel -> unit
      val copy_file : string -> string -> unit
      val print_string_list : Format.formatter -> string list -> unit
      val ( !* ) : 'a Lazy.t -> 'a
      val ( & ) : ('a -> 'b) -> 'a -> 'b
      val ( |> ) : 'a -> ('a -> 'b) -> 'b
      val ( @:= ) : 'a list Pervasives.ref -> 'a list -> unit
      val memo : ('a -> 'b) -> 'a -> 'b
    end
  module type OPTIONS =
    sig
      type command_spec
      val build_dir : string Pervasives.ref
      val include_dirs : string list Pervasives.ref
      val exclude_dirs : string list Pervasives.ref
      val nothing_should_be_rebuilt : bool Pervasives.ref
      val ocamlc : command_spec Pervasives.ref
      val ocamlopt : command_spec Pervasives.ref
      val ocamldep : command_spec Pervasives.ref
      val ocamldoc : command_spec Pervasives.ref
      val ocamlyacc : command_spec Pervasives.ref
      val ocamllex : command_spec Pervasives.ref
      val ocamlrun : command_spec Pervasives.ref
      val ocamlmklib : command_spec Pervasives.ref
      val ocamlmktop : command_spec Pervasives.ref
      val hygiene : bool Pervasives.ref
      val sanitize : bool Pervasives.ref
      val sanitization_script : string Pervasives.ref
      val ignore_auto : bool Pervasives.ref
      val plugin : bool Pervasives.ref
      val just_plugin : bool Pervasives.ref
      val native_plugin : bool Pervasives.ref
      val make_links : bool Pervasives.ref
      val nostdlib : bool Pervasives.ref
      val program_to_execute : bool Pervasives.ref
      val must_clean : bool Pervasives.ref
      val catch_errors : bool Pervasives.ref
      val use_menhir : bool Pervasives.ref
      val show_documentation : bool Pervasives.ref
      val recursive : bool Pervasives.ref
      val targets : string list Pervasives.ref
      val ocaml_libs : string list Pervasives.ref
      val ocaml_cflags : string list Pervasives.ref
      val ocaml_lflags : string list Pervasives.ref
      val ocaml_ppflags : string list Pervasives.ref
      val ocaml_yaccflags : string list Pervasives.ref
      val ocaml_lexflags : string list Pervasives.ref
      val program_args : string list Pervasives.ref
      val ignore_list : string list Pervasives.ref
      val tags : string list Pervasives.ref
      val tag_lines : string list Pervasives.ref
      val show_tags : string list Pervasives.ref
      val ext_obj : string Pervasives.ref
      val ext_lib : string Pervasives.ref
      val ext_dll : string Pervasives.ref
    end
  module type ARCH =
    sig
      type 'a arch = private
          Arch_dir of string * 'a * 'a arch list
        | Arch_dir_pack of string * 'a * 'a arch list
        | Arch_file of string * 'a
      val dir :
        string -> unit arch list -> unit arch
      val dir_pack :
        string -> unit arch list -> unit arch
      val file : string -> unit arch
      type info = private {
        current_path : string;
        include_dirs : string list;
        for_pack : string;
      }
      val annotate :
        'a arch -> info arch
      val print :
        (Format.formatter -> 'a -> unit) ->
        Format.formatter -> 'a arch -> unit
      val print_include_dirs : Format.formatter -> string list -> unit
      val print_info : Format.formatter -> info -> unit
      val iter_info : ('a -> unit) -> 'a arch -> unit
      val fold_info : ('a -> 'b -> 'b) -> 'a arch -> 'b -> 'b
      val iter_include_dirs :
        info arch -> (string -> unit) -> unit
      val mk_tables :
        info arch ->
        (string, string list) Hashtbl.t * (string, string) Hashtbl.t
      val print_table :
        (Format.formatter -> 'a -> unit) ->
        Format.formatter -> (string, 'a) Hashtbl.t -> unit
    end
  module type PLUGIN =
    sig
      module Pathname : PATHNAME
      module Tags : TAGS
      module Command :
        sig
          type tags = Tags.t
          type pathname = Pathname.t
          type t =
              Seq of t list
            | Cmd of spec
            | Echo of string list * pathname
            | Nop
          and spec =
              N
            | S of spec list
            | A of string
            | P of pathname
            | Px of pathname
            | Sh of string
            | T of tags
            | V of string
            | Quote of spec
          val atomize : string list -> spec
          val atomize_paths : string list -> spec
          val execute : ?quiet:bool -> ?pretend:bool -> t -> unit
          val execute_many :
            ?quiet:bool ->
            ?pretend:bool -> t list -> (bool list * exn) option
          val setup_virtual_command_solver : string -> (unit -> spec) -> unit
          val search_in_path : string -> string
          val reduce : spec -> spec
          val print : Format.formatter -> t -> unit
          val to_string : t -> string
          val string_of_command_spec : spec -> string
        end
      module Outcome : OUTCOME
      module String : STRING
      module List : LIST
      module StringSet :
        sig
          type elt = String.t
          type t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val singleton : elt -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val max_elt : t -> elt
          val choose : t -> elt
          val split : elt -> t -> t * bool * t
        end
      module Options :
        sig
          type command_spec = Command.spec
          val build_dir : string ref
          val include_dirs : string list ref
          val exclude_dirs : string list ref
          val nothing_should_be_rebuilt : bool ref
          val ocamlc : command_spec ref
          val ocamlopt : command_spec ref
          val ocamldep : command_spec ref
          val ocamldoc : command_spec ref
          val ocamlyacc : command_spec ref
          val ocamllex : command_spec ref
          val ocamlrun : command_spec ref
          val ocamlmklib : command_spec ref
          val ocamlmktop : command_spec ref
          val hygiene : bool ref
          val sanitize : bool ref
          val sanitization_script : string ref
          val ignore_auto : bool ref
          val plugin : bool ref
          val just_plugin : bool ref
          val native_plugin : bool ref
          val make_links : bool ref
          val nostdlib : bool ref
          val program_to_execute : bool ref
          val must_clean : bool ref
          val catch_errors : bool ref
          val use_menhir : bool ref
          val show_documentation : bool ref
          val recursive : bool ref
          val targets : string list ref
          val ocaml_libs : string list ref
          val ocaml_cflags : string list ref
          val ocaml_lflags : string list ref
          val ocaml_ppflags : string list ref
          val ocaml_yaccflags : string list ref
          val ocaml_lexflags : string list ref
          val program_args : string list ref
          val ignore_list : string list ref
          val tags : string list ref
          val tag_lines : string list ref
          val show_tags : string list ref
          val ext_obj : string ref
          val ext_lib : string ref
          val ext_dll : string ref
        end
      module Arch : ARCH
      val opt_print :
        (Format.formatter -> 'a -> unit) ->
        Format.formatter -> 'a option -> unit
      val the : 'a option -> 'a
      val getenv : ?default:string -> string -> string
      val with_input_file : ?bin:bool -> string -> (in_channel -> 'a) -> 'a
      val with_output_file : ?bin:bool -> string -> (out_channel -> 'a) -> 'a
      val with_temp_file : string -> string -> (string -> 'a) -> 'a
      val read_file : string -> string
      val copy_chan : in_channel -> out_channel -> unit
      val copy_file : string -> string -> unit
      val print_string_list : Format.formatter -> string list -> unit
      val ( !* ) : 'a Lazy.t -> 'a
      val ( & ) : ('a -> 'b) -> 'a -> 'b
      val ( |> ) : 'a -> ('a -> 'b) -> 'b
      val ( @:= ) : 'a list ref -> 'a list -> unit
      val memo : ('a -> 'b) -> 'a -> 'b
      type command =
        Command.t =
          Seq of command list
        | Cmd of spec
        | Echo of string list * Pathname.t
        | Nop
      and spec =
        Command.spec =
          N
        | S of spec list
        | A of string
        | P of string
        | Px of string
        | Sh of string
        | T of Tags.t
        | V of string
        | Quote of spec
      val ( / ) : Pathname.t -> Pathname.t -> Pathname.t
      val ( -.- ) : Pathname.t -> string -> Pathname.t
      val ( ++ ) : Tags.t -> Tags.elt -> Tags.t
      val ( -- ) : Tags.t -> Tags.elt -> Tags.t
      val ( +++ ) : Tags.t -> Tags.elt option -> Tags.t
      val ( --- ) : Tags.t -> Tags.elt option -> Tags.t
      type env = Pathname.t -> Pathname.t
      type builder = Pathname.t list list -> (Pathname.t, exn) Outcome.t list
      type action =
          env ->
          builder -> Command.t
      val rule :
        string ->
        ?tags:string list ->
        ?prods:string list ->
        ?deps:string list ->
        ?prod:string ->
        ?dep:string ->
        ?stamp:string ->
        ?insert:[ `after of string | `before of string | `bottom | `top ] ->
        action -> unit
      val copy_rule :
        string ->
        ?insert:[ `after of string | `before of string | `bottom | `top ] ->
        string -> string -> unit
      val dep : Tags.elt list -> Pathname.t list -> unit
      val flag : Tags.elt list -> Command.spec -> unit
      val flag_and_dep :
        Tags.elt list -> Command.spec -> unit
      val non_dependency : Pathname.t -> string -> unit
      val use_lib : Pathname.t -> Pathname.t -> unit
      val ocaml_lib :
        ?extern:bool ->
        ?byte:bool ->
        ?native:bool ->
        ?dir:Pathname.t -> ?tag_name:string -> Pathname.t -> unit
      val expand_module :
        Pathname.t list -> Pathname.t -> string list -> Pathname.t list
      val string_list_of_file : Pathname.t -> string list
      val module_name_of_pathname : Pathname.t -> string
      val mv : Pathname.t -> Pathname.t -> Command.t
      val cp : Pathname.t -> Pathname.t -> Command.t
      val ln_f : Pathname.t -> Pathname.t -> Command.t
      val ln_s : Pathname.t -> Pathname.t -> Command.t
      val rm_f : Pathname.t -> Command.t
      val chmod :
        Command.spec ->
        Pathname.t -> Command.t
      val cmp : Pathname.t -> Pathname.t -> Command.t
      val hide_package_contents : string -> unit
      val tag_file : Pathname.t -> Tags.elt list -> unit
      val tag_any : Tags.elt list -> unit
      val tags_of_pathname : Pathname.t -> Tags.t
      type hook =
          Before_hygiene
        | After_hygiene
        | Before_options
        | After_options
        | Before_rules
        | After_rules
      val dispatch : (hook -> unit) -> unit
    end
end



module Make = 
	functor (Ocamlbuild_plugin: OcamlbuildSignatures.PLUGIN) ->
struct
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
	    Cmd(S[A "lib.exe" ; A "/NOLOGO" ; A ("/OUT:" ^ archive) ; T tags; S spec_obj_list ])
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
    flag [ "compile" ; "c++" ; "msvc" ; "release" ] (A "/Ox") ; 

    flag [ "link" ; "shared" ; "linux" ] (A "-shared") ;
    flag [ "link" ; "shared" ; "mac" ] (A "-shared") ;

    flag [ "link" ; "shared" ; "mac" ; "clang" ]
      (S [A "-flat_namespace" ; A "-undefined" ; A "suppress"]) ;

    flag [ "link" ; "shared" ; "windows" ; "msvc" ]
      (S [A "-chain" ; A "msvc" ; A "-merge-manifest" ])
end