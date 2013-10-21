(** Informations about current OS *)
(* should-it be private ? *)
type t = 
  | Linux 
  | Windows 
  | Mac
      

(** Current OS in use as reported by the c++ compiler *)
val current : t
  
(** name of the os, that is either "linux", "windows" or "mac" *)
val name : t -> string

