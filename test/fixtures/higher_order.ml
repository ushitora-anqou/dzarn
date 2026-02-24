(* Test function usage in higher-order function contexts *)

(* Function passed as argument (callback) *)
let process_list f lst = List.map f lst
let add_five x = x + 5

let use_map () =
  let _ = process_list add_five [ 1; 2; 3 ] in
  ()

(* Function used with List.fold_left *)
let summer acc x = acc + x

let use_fold () =
  let _ = List.fold_left summer 0 [ 1; 2; 3 ] in
  ()

(* Function used with List.filter *)
let is_positive x = x > 0

let use_filter () =
  let _ = List.filter is_positive [ 1; -2; 3 ] in
  ()

(* Function used as argument to another function *)
let apply_transform f x = f x
let square x = x * x

let use_apply () =
  let _ = apply_transform square 5 in
  ()

(* Main function *)
let main () =
  let _ = use_map () in
  let _ = use_fold () in
  let _ = use_filter () in
  let _ = use_apply () in
  ()
