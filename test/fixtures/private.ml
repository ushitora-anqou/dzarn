(* Test file with private (underscore-prefixed) functions *)
let public_function x = x + 1
let _private_helper y = y * 2

let () =
  let _ = public_function 10 in
  ()
