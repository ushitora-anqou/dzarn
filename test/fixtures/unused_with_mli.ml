(* Test fixture for .ml/.mli pair fix functionality *)

(* This function is declared in .mli but unused - should be removed from both files *)
let unused_public x = x + 1

(* This function is declared in .mli and used - should remain *)
let used_public y = y * 2

(* This function is NOT in .mli (private) - should not be affected *)
let private_func z = z + 3

let () =
  let _ = used_public 5 in
  ()
