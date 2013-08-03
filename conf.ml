module OS =
struct
  type t = Linux | Windows | Mac
  external detect : unit -> t = "conf_os" "noalloc"
  let current = detect ()

  let name = function
    | Linux -> "linux"
    | Windows -> "windows"
    | Mac -> "mac"
end
 
