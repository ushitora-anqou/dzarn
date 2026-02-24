(* Configuration file parser using sexplib *)

[@@@warning "-27"]

open Sexplib.Std

type t = {
  unused_enabled : bool;
  complexity_enabled : bool;
  complexity_threshold : int;
}
[@@deriving sexp]

let default =
  {
    unused_enabled = true;
    complexity_enabled = true;
    complexity_threshold = 10;
  }

let parse_file filename =
  let ic = open_in filename in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let content = read_all "" in
  close_in ic;
  (* Parse using sexplib *)
  t_of_sexp (Sexplib.Sexp.of_string content)

let parse_string content = t_of_sexp (Sexplib.Sexp.of_string content)
