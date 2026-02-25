(* Configuration file parser using sexplib *)

[@@@warning "-27"]

open Sexplib.Std

type t = {
  unused_enabled : bool;
  complexity_enabled : bool;
  complexity_threshold : int;
  naming_enabled : bool;
}
[@@deriving sexp]

let default =
  {
    unused_enabled = true;
    complexity_enabled = true;
    complexity_threshold = 10;
    naming_enabled = true;
  }

let parse_file filename =
  let ic = open_in filename in
  let buf = Buffer.create 4096 in
  let rec read_all () =
    try
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
      read_all ()
    with End_of_file -> ()
  in
  (* Protect against resource leaks *)
  try
    read_all ();
    close_in ic;
    let content = Buffer.contents buf in
    (* Parse using sexplib *)
    t_of_sexp (Sexplib.Sexp.of_string content)
  with e ->
    close_in ic;
    raise e

let parse_string content = t_of_sexp (Sexplib.Sexp.of_string content)
