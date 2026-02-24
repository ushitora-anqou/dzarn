(* Test function usage in record contexts *)

(* Function used in record field expression *)
(* This tests that Pexp_record field values are traversed *)
let transform_x x = x * 2
let create_record x = { x = transform_x x }

(* Function used when destructuring records *)
let extract_y { y; _ } = y
let use_destruct r = extract_y r

(* Function used in record update *)
let increment y = y + 1
let update_y r = { r with y = increment r.y }

(* Main function *)
let main () =
  let r = create_record 5 in
  let _ = use_destruct r in
  let _ = update_y r in
  ()
