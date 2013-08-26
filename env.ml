
let get var =
  Sys.getenv var

let get_opt var =
  try Some (Sys.getenv var)
  with Not_found -> None

let get_default var def =
  try Sys.getenv var
  with Not_found -> def

let do_opt var f =
  try 
    let p = Sys.getenv var in
    Some (f p)
  with Not_found -> None

let do_default var f def =
  try 
    let p = Sys.getenv var in
    (f p)
  with Not_found -> def
