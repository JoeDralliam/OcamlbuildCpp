
let get var =
  Sys.getenv var

let get_optional var =
  try Some (Sys.getenv var)
  with Not_found -> None

let get_with_default var def =
  try Sys.getenv var
  with Not_found -> def

let do_optional var f =
  try 
    let p = Sys.getenv var in
    Some (f p)
  with Not_found -> None

let do_with_default var f def =
  try 
    let p = Sys.getenv var in
    (f p)
  with Not_found -> def

