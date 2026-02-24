(* Implementation file - only functions declared in .mli are public *)
let declared_public x = x + 1
let used_public y = y * 2
let undeclared_private z = z + 3

let () =
  let _ = used_public 5 in
  ()
