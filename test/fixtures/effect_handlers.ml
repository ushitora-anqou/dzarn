(* Effect handlers test for OCaml 5+ *)
(* Tests that functions called in effect handler bodies are tracked *)

open Effect
open Effect.Deep

(* 1. Simple effect declaration *)
type _ t += Echo : string -> unit t

(* 2. Function that performs the effect *)
let echo_message () = perform (Echo "hello")

(* 3. Helper function called in effect handler body *)
let log_message msg = Printf.printf "LOG: %s\n" msg

(* 4. Effect handler that calls log_message in its body *)
let run_echo () =
  try echo_message ()
  with effect Echo msg, k ->
    log_message msg;
    continue k ()

(* 5. State effects *)
type _ t += Get : unit -> int t
type _ t += Put : int -> unit t

(* 6. Helper functions called in effect handler bodies *)
let get_current_state state_ref = !state_ref
let update_state state_ref new_value = state_ref := new_value

(* 7. State handler that calls helper functions *)
let[@nolint] run_state (initial : int) (f : unit -> 'a) : 'a =
  let state = ref initial in
  try f () with
  | effect Get (), k ->
      let current = get_current_state state in
      continue k current
  | effect Put v, k ->
      update_state state v;
      continue k ()

(* 8. Functions that use state effects *)
let increment () =
  let current = perform (Get ()) in
  perform (Put (current + 1))

let get_and_double () =
  let current = perform (Get ()) in
  perform (Put (current * 2))

(* 9. Main function using state handler *)
let main () =
  run_state 0 (fun () ->
      increment ();
      get_and_double ())

(* 10. Choice effect *)
type _ t += Choice : bool t

let perform_choice () = perform Choice

(* 11. Helper called in effect handler body *)
let handle_both_choices k =
  continue k true;
  continue k false

(* 12. Choice handler that calls helper *)
let run_choice (f : unit -> int) : int option =
  try Some (f ()) with effect Choice, k -> handle_both_choices k

(* 13. Used function - called in effect handler body *)
let format_echo_msg msg = "Echo: " ^ msg

let used_in_handler () =
  try echo_message ()
  with effect Echo msg, k ->
    Printf.printf "%s\n" (format_echo_msg msg);
    continue k ()

(* 14. Unused function - never called *)
let unused_function x = x + 1
