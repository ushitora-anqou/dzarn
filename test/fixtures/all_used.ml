(* Test file where all functions are used *)
let helper x = x + 1

let main value =
  let transformed = helper value in
  transformed * 2

let () =
  let _ = main 10 in
  ()
