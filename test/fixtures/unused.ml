(* Test file with an unused function *)
let used_function x = x + 1
let unused_function y = y * 2

let () =
  let _ = used_function 10 in
  ()
