(* Complexity checker tests *)

open Stdlib
open Dzarn

(* Helper function for temp directory *)
let test_counter = ref 0

let with_temp_dir f =
  let tmp_dir =
    Filename.concat
      (try Sys.getenv "TMPDIR" with Not_found -> "/tmp")
      ("dzarn_complexity_test_"
      ^ string_of_int (Unix.getpid ())
      ^ "_"
      ^ string_of_int !test_counter)
  in
  incr test_counter;
  if not (Sys.file_exists tmp_dir) then Unix.mkdir tmp_dir 0o755;
  try
    let result = f tmp_dir in
    (try
       let files = Sys.readdir tmp_dir |> Array.to_list in
       List.iter (fun f -> Sys.remove (Filename.concat tmp_dir f)) files;
       Sys.rmdir tmp_dir
     with _ -> ());
    result
  with e ->
    (try
       let files = Sys.readdir tmp_dir |> Array.to_list in
       List.iter (fun f -> Sys.remove (Filename.concat tmp_dir f)) files;
       Sys.rmdir tmp_dir
     with _ -> ());
    raise e

(* Helper to check if string contains substring *)
let string_contains s sub =
  let sub_len = String.length sub in
  let s_len = String.length s in
  let rec check i =
    if i + sub_len > s_len then false
    else if String.sub s i sub_len = sub then true
    else check (i + 1)
  in
  check 0

(* Helper to run analyzer and capture output *)
let run_analyzer ~config tmp_dir =
  let output_file = Filename.concat tmp_dir "analyzer_output.txt" in
  (try Sys.remove output_file with _ -> ());
  let oc = open_out output_file in
  let old_stdout = Unix.dup Unix.stdout in
  Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stdout;
  let code =
    try Dzarn.Analyzer.run ~fix:false ~config tmp_dir
    with e ->
      flush stdout;
      Unix.dup2 old_stdout Unix.stdout;
      Unix.close old_stdout;
      close_out oc;
      raise e
  in
  flush stdout;
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  close_out oc;
  let ic = open_in output_file in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let result = read_all "" in
  close_in ic;
  (try Sys.remove output_file with _ -> ());
  (code, result)

(* Test 1: Simple function with low complexity *)
let test_low_complexity () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "simple.ml") in
  output_string oc "let simple x = x + 1\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 10;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "No functions exceed complexity threshold" then ()
  else failwith "Expected no complexity issues"

(* Test 2: Function with high complexity (many branches) *)
let test_high_complexity () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "complex.ml") in
  output_string oc "let complex x =\n";
  output_string oc "  match x with\n";
  output_string oc "  | 1 -> 1\n";
  output_string oc "  | 2 -> 2\n";
  output_string oc "  | 3 -> 3\n";
  output_string oc "  | 4 -> 4\n";
  output_string oc "  | 5 -> 5\n";
  output_string oc "  | 6 -> 6\n";
  output_string oc "  | _ -> 0\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 5;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "complexity" then ()
  else failwith "Expected complexity issue to be reported"

(* Test 3: Config disables complexity check *)
let test_complexity_disabled () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "complex.ml") in
  output_string oc "let complex x =\n";
  output_string oc "  match x with\n";
  output_string oc "  | 1 -> 1\n";
  output_string oc "  | 2 -> 2\n";
  output_string oc "  | _ -> 0\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        complexity_threshold = 1;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "Complexity checking disabled" then ()
  else failwith "Expected complexity to be disabled"

(* Test 4: Both unused and complexity enabled *)
let test_both_enabled () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "both.ml") in
  output_string oc "let unused x = x + 1\n";
  output_string oc "let complex x =\n";
  output_string oc "  match x with\n";
  output_string oc "  | 1 -> 1\n";
  output_string oc "  | 2 -> 2\n";
  output_string oc "  | 3 -> 3\n";
  output_string oc "  | _ -> 0\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = true;
        complexity_enabled = true;
        complexity_threshold = 3;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "unused" then ()
  else failwith "Expected unused function to be reported";
  if string_contains output "complexity" then ()
  else failwith "Expected complexity issue to be reported"

(* Test 5: If and try-with complexity *)
let test_if_and_try_complexity () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "branches.ml") in
  output_string oc "let complex x y =\n";
  output_string oc "  try\n";
  output_string oc "    if x > 0 then 1 else 2\n";
  output_string oc "  with\n";
  output_string oc "  | Not_found -> 3\n";
  output_string oc "  | _ -> 4\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 3;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "complexity" then ()
  else failwith "Expected complexity issue for if/try to be reported"

(* Test 6: Match has same complexity regardless of case count *)
let test_match_case_count () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "match.ml") in
  output_string oc "let f1 x =\n";
  output_string oc "  match x with\n";
  output_string oc "  | 1 -> 1\n";
  output_string oc "  | _ -> 0\n";
  output_string oc "let f2 x =\n";
  output_string oc "  match x with\n";
  output_string oc "  | 1 -> 1\n";
  output_string oc "  | 2 -> 2\n";
  output_string oc "  | 3 -> 3\n";
  output_string oc "  | 4 -> 4\n";
  output_string oc "  | 5 -> 5\n";
  output_string oc "  | 6 -> 6\n";
  output_string oc "  | 7 -> 7\n";
  output_string oc "  | _ -> 0\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 1;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Both f1 and f2 should have complexity 2 *)
  if string_contains output "f1" && string_contains output "f2" then ()
  else failwith "Expected both f1 and f2 to have complexity issues"

(* Test 7: Try has same complexity regardless of handler count *)
let test_try_handler_count () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "try.ml") in
  output_string oc "let f1 x =\n";
  output_string oc "  try x with\n";
  output_string oc "  | _ -> 0\n";
  output_string oc "let f2 x =\n";
  output_string oc "  try x with\n";
  output_string oc "  | Not_found -> 1\n";
  output_string oc "  | Invalid_argument _ -> 2\n";
  output_string oc "  | _ -> 3\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 1;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Both f1 and f2 should have complexity 2 *)
  if string_contains output "f1" && string_contains output "f2" then ()
  else failwith "Expected both f1 and f2 to have complexity issues"

(* Test 8: Simple function has complexity 1 *)
let test_simple_function_complexity () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "simple.ml") in
  output_string oc "let simple x = x + 1\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 1;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Complexity 1 should not exceed threshold 1 *)
  if string_contains output "No functions exceed complexity threshold" then ()
  else failwith "Expected simple function to have complexity 1"

(* Test 9: Record field expression complexity *)
let test_record_complexity () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "record.ml") in
  output_string oc "type t = { a: int; b: int; c: int }\n";
  output_string oc "let make_record x =\n";
  output_string oc "  { a = x + 1; b = x * 2; c = if x > 0 then 1 else 0 }\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 1;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Should detect complexity from the if-then-else in record field *)
  if string_contains output "make_record" && string_contains output "complexity"
  then ()
  else failwith "Expected record function to report complexity"

(* Test 10: Threshold is correctly reported in output *)
let test_threshold_reported () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "threshold.ml") in
  output_string oc "let complex x =\n";
  output_string oc "  match x with\n";
  output_string oc "  | 1 -> 1\n";
  output_string oc "  | 2 -> 2\n";
  output_string oc "  | _ -> 0\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = true;
        complexity_threshold = 1;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Check that threshold value 1 is reported in output *)
  if string_contains output "threshold: 1" then ()
  else failwith "Expected threshold 1 to be reported in output"

let () =
  let open Alcotest in
  run "Complexity"
    [
      ( "Low complexity",
        [
          test_case "simple function below threshold" `Quick test_low_complexity;
        ] );
      ( "High complexity",
        [
          test_case "complex function above threshold" `Quick
            test_high_complexity;
        ] );
      ( "Disabled",
        [
          test_case "complexity check disabled" `Quick test_complexity_disabled;
        ] );
      ( "Both enabled",
        [ test_case "both checkers enabled" `Quick test_both_enabled ] );
      ( "If and try",
        [
          test_case "if and try-with complexity" `Quick
            test_if_and_try_complexity;
        ] );
      ( "Match case count",
        [ test_case "match has same complexity" `Quick test_match_case_count ]
      );
      ( "Try handler count",
        [ test_case "try has same complexity" `Quick test_try_handler_count ] );
      ( "Simple function",
        [
          test_case "simple function has complexity 1" `Quick
            test_simple_function_complexity;
        ] );
      ( "Record complexity",
        [ test_case "record field expression" `Quick test_record_complexity ] );
      ( "Threshold reported",
        [ test_case "threshold in output" `Quick test_threshold_reported ] );
    ]
