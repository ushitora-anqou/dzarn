(* Bug reproduction fixture for opened module function detection *)

(* Current module defines a 'map' function *)
let map f lst = List.map f lst

(* This function is unused - should be reported as unused *)
let unused_func x = x + 1

(* Use map via open List *)
let use_opened_map () =
  let open List in
  let _ = map (fun x -> x * 2) [ 1; 2; 3 ] in
  ()

(* Use map via let open *)
let use_let_open () =
  let open List in
  let _ = map (fun x -> x + 1) [ 4; 5; 6 ] in
  ()

(* Use map via dot notation *)
let use_dot_notation () =
  let _ = List.(map (fun x -> x - 1) [ 7; 8; 9 ]) in
  ()
