(* Module B - uses function from Module A *)
let public_b x = A.public_a x + 1
let unused_b y = y * 2

let () =
  let _ = public_b 5 in
  ()
