(* Function length checker tests *)

open Stdlib
open Dzarn

(* Helper function for temp directory *)
let test_counter = ref 0

let with_temp_dir f =
  let tmp_dir =
    Filename.concat
      (try Sys.getenv "TMPDIR" with Not_found -> "/tmp")
      ("dzarn_length_test_"
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

(* Test 1: Simple function with few lines *)
let test_short_function () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "short.ml") in
  output_string oc "let short x = x + 1\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 50;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "No functions exceed line count threshold" then ()
  else failwith "Expected no length issues"

(* Test 2: Function with many lines *)
let test_long_function () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "long.ml") in
  output_string oc "let long x =\n";
  for _ = 1 to 10 do
    output_string oc "  let _ = x + 1 in\n"
  done;
  output_string oc "  x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 5;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "long" && string_contains output "lines" then ()
  else failwith "Expected length issue to be reported"

(* Test 3: Config disables length check *)
let test_length_disabled () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "long.ml") in
  output_string oc "let long x =\n";
  for _ = 1 to 10 do
    output_string oc "  let _ = x + 1 in\n"
  done;
  output_string oc "  x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = false;
        length_threshold = 1;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  if string_contains output "Length checking disabled" then ()
  else failwith "Expected length to be disabled"

(* Test 4: Multiple functions, only long ones reported *)
let test_mixed_functions () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "mixed.ml") in
  output_string oc "let short x = x + 1\n";
  output_string oc "let long x =\n";
  for _ = 1 to 10 do
    output_string oc "  let _ = x + 1 in\n"
  done;
  output_string oc "  x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 5;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* short should not be reported, long should be reported *)
  if string_contains output "long" && not (string_contains output "short") then
    ()
  else failwith "Expected only long function to be reported, not short function"

(* Test 5: Threshold is correctly reported in output *)
let test_threshold_reported () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "threshold.ml") in
  output_string oc "let long x =\n";
  for _ = 1 to 10 do
    output_string oc "  let _ = x + 1 in\n"
  done;
  output_string oc "  x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 5;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Check that threshold value 5 is reported in output *)
  if string_contains output "threshold: 5" then ()
  else failwith "Expected threshold 5 to be reported in output"

(* Test 6: Line count is correctly reported *)
let test_line_count_reported () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "count.ml") in
  output_string oc "let count x =\n";
  output_string oc "  let a = x + 1 in\n";
  output_string oc "  let b = x + 2 in\n";
  output_string oc "  let c = x + 3 in\n";
  output_string oc "  x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 3;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Check that line count is reported in output *)
  if string_contains output "5 lines" then ()
  else failwith "Expected line count to be reported in output"

(* Test 7: Nested function that exceeds threshold *)
let test_nested_long_function () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "nested.ml") in
  output_string oc "let outer x =\n";
  output_string oc "  let inner y =\n";
  for _ = 1 to 10 do
    output_string oc "    let _ = y + 1 in\n"
  done;
  output_string oc "    y\n";
  output_string oc "  in\n";
  output_string oc "  inner x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 5;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Both inner and outer are reported since outer's expression spans all lines *)
  if string_contains output "inner" then ()
  else failwith "Expected nested function 'inner' to be reported";
  if string_contains output "outer" then ()
  else failwith "Expected outer function to be reported (contains nested code)"

(* Test 8: Both outer and nested functions exceed threshold *)
let test_nested_both_long () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "both.ml") in
  output_string oc "let outer x =\n";
  for _ = 1 to 10 do
    output_string oc "  let _ = x + 1 in\n"
  done;
  output_string oc "  let inner y =\n";
  for _ = 1 to 10 do
    output_string oc "    let _ = y + 1 in\n"
  done;
  output_string oc "    y\n";
  output_string oc "  in\n";
  output_string oc "  inner x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 5;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* Both outer and inner should be reported *)
  if string_contains output "outer" && string_contains output "inner" then ()
  else failwith "Expected both outer and nested function to be reported"

(* Test 9: Short outer with long nested function *)
let test_nested_outer_short_inner_long () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "mixed_nested.ml") in
  output_string oc "let outer x = inner x\n";
  output_string oc "and inner y =\n";
  for _ = 1 to 10 do
    output_string oc "  let _ = y + 1 in\n"
  done;
  output_string oc "  y\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 5;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* inner should be reported, outer should not *)
  if string_contains output "inner" && not (string_contains output "outer") then
    ()
  else failwith "Expected only inner function to be reported"

(* Test 10: Deeply nested functions *)
let test_deeply_nested_functions () =
  with_temp_dir @@ fun tmp_dir ->
  let oc = open_out (Filename.concat tmp_dir "deep.ml") in
  output_string oc "let level1 x =\n";
  output_string oc "  let level2 y =\n";
  output_string oc "    let level3 z =\n";
  for _ = 1 to 10 do
    output_string oc "        let _ = z + 1 in\n"
  done;
  output_string oc "        z\n";
  output_string oc "    in\n";
  output_string oc "    level3 y\n";
  output_string oc "  in\n";
  output_string oc "  level2 x\n";
  close_out oc;
  let config =
    Config.
      {
        unused_enabled = false;
        complexity_enabled = false;
        naming_enabled = false;
        length_enabled = true;
        length_threshold = 5;
        complexity_threshold = 10;
        unused_nolint_enabled = false;
        json_output = false;
      }
  in
  let _, output = run_analyzer ~config tmp_dir in
  (* All levels are reported since each spans multiple lines *)
  if string_contains output "level3" then ()
  else failwith "Expected deeply nested function 'level3' to be reported";
  if string_contains output "level2" then ()
  else failwith "Expected level2 function to be reported";
  if string_contains output "level1" then ()
  else failwith "Expected level1 function to be reported"

let () =
  let open Alcotest in
  run "Length"
    [
      ( "Short function",
        [ test_case "function below threshold" `Quick test_short_function ] );
      ( "Long function",
        [ test_case "function above threshold" `Quick test_long_function ] );
      ( "Disabled",
        [ test_case "length check disabled" `Quick test_length_disabled ] );
      ( "Mixed functions",
        [ test_case "only long functions reported" `Quick test_mixed_functions ]
      );
      ( "Threshold reported",
        [ test_case "threshold in output" `Quick test_threshold_reported ] );
      ( "Line count reported",
        [ test_case "line count in output" `Quick test_line_count_reported ] );
      ( "Nested functions",
        [
          test_case "nested function detection" `Quick test_nested_long_function;
        ] );
      ( "Nested both long",
        [ test_case "both outer and nested long" `Quick test_nested_both_long ]
      );
      ( "Nested outer short inner long",
        [
          test_case "short outer long inner" `Quick
            test_nested_outer_short_inner_long;
        ] );
      ( "Deeply nested",
        [
          test_case "all nested levels detected" `Quick
            test_deeply_nested_functions;
        ] );
    ]
