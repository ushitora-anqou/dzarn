open Stdlib

(* Helper function to create a temporary test directory *)
let test_counter = ref 0

let with_temp_dir f =
  let tmp_dir =
    Filename.concat
      (try Sys.getenv "TMPDIR" with Not_found -> "/tmp")
      ("dzarn_test_"
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

(* Helper function to get the fixtures path *)
let fixtures_path =
  let cwd = Sys.getcwd () in
  (* Check if we're in the build directory - simple heuristic *)
  let is_in_build =
    let rec check dir =
      if dir = "" then false
      else if Filename.basename dir = "_build" then true
      else if Filename.basename dir = "default" then
        check (Filename.dirname dir)
      else check (Filename.dirname dir)
    in
    check cwd
  in
  if is_in_build then
    (* We're in the build directory, go up to find source fixtures *)
    let rec go_up_to_source dir =
      (* Find the _build directory and go up to project root *)
      if Filename.basename dir = "_build" then
        Filename.concat (Filename.dirname dir) "test/fixtures"
      else if Filename.basename dir = "default" then
        go_up_to_source (Filename.dirname dir)
      else go_up_to_source (Filename.dirname dir)
    in
    go_up_to_source cwd
  else
    (* We're in the source directory *)
    (* Get the project root by going up from the current directory *)
    let rec find_root dir =
      if Sys.file_exists (Filename.concat dir "dzarn.opam") then dir
      else
        let parent = Filename.dirname dir in
        if parent = dir then cwd else find_root parent
    in
    let root = find_root cwd in
    Filename.concat root "test/fixtures"

(* Helper function to copy a file to temp directory *)
let copy_file src dest =
  let src_path = Filename.concat fixtures_path src in
  let ic = open_in_bin src_path in
  let oc = open_out_bin dest in
  try
    let buffer = Bytes.create 4096 in
    let rec copy () =
      let n = input ic buffer 0 4096 in
      if n > 0 then (
        output oc buffer 0 n;
        copy ())
    in
    copy ();
    close_in ic;
    close_out oc
  with e ->
    close_in ic;
    close_out oc;
    raise e

(* Helper to check if a string contains a substring *)
let string_contains s sub =
  let sub_len = String.length sub in
  let s_len = String.length s in
  let rec check i =
    if i + sub_len > s_len then false
    else if String.sub s i sub_len = sub then true
    else check (i + 1)
  in
  check 0

(* Helper to capture output from Analyzer.run *)
let run_analyzer ~fix dir =
  let output_file = Filename.concat dir "analyzer_output.txt" in
  (* Remove any existing output file *)
  (try Sys.remove output_file with _ -> ());
  let exit_code =
    (* Redirect stdout to file *)
    let oc = open_out output_file in
    let old_stdout = Unix.dup Unix.stdout in
    Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stdout;
    let config = Dzarn.Config.default in
    let code =
      try Dzarn.Analyzer.run ~fix ~config dir
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
    code
  in

  (* Read output from file *)
  let output =
    let ic = open_in output_file in
    let rec read_all acc =
      try
        let line = input_line ic in
        read_all (acc ^ line ^ "\n")
      with End_of_file -> acc
    in
    let result = read_all "" in
    close_in ic;
    result
  in
  (try Sys.remove output_file with _ -> ());
  (exit_code, output)

(* Test 1: Detect unused function *)
let test_detect_unused () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "unused.ml" (Filename.concat tmp_dir "unused.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  if string_contains output "unused_function" then ()
  else failwith "Expected output to contain 'unused_function'"

(* Test 2: Private functions (underscore prefix) should not be reported *)
let test_private_functions () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "private.ml" (Filename.concat tmp_dir "private.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  if not (string_contains output "_private_helper") then ()
  else failwith "Private function should not be reported"

(* Test 3: Functions declared in .mli are public *)
let test_mli_declared () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "with_mli.ml" (Filename.concat tmp_dir "with_mli.ml");
  copy_file "with_mli.mli" (Filename.concat tmp_dir "with_mli.mli");
  let _, output = run_analyzer ~fix:false tmp_dir in
  if string_contains output "declared_public" then ()
  else failwith "Expected output to contain 'declared_public'";
  if not (string_contains output "undeclared_private") then ()
  else failwith "Function not in .mli should not be reported as unused"

(* Test 4: All functions used - should report no unused functions *)
let test_all_used () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "all_used.ml" (Filename.concat tmp_dir "all_used.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  if string_contains output "No unused public functions found" then ()
  else failwith "Expected no unused functions"

(* Test 5: Multi-file project *)
let test_multi_file () =
  with_temp_dir @@ fun tmp_dir ->
  Unix.mkdir (Filename.concat tmp_dir "multi") 0o755;
  copy_file "multi/a.ml" (Filename.concat tmp_dir "multi/a.ml");
  copy_file "multi/b.ml" (Filename.concat tmp_dir "multi/b.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  let has_unused_a = string_contains output "unused_a" in
  let has_unused_b = string_contains output "unused_b" in
  if has_unused_a && has_unused_b then ()
  else
    failwith
      (Printf.sprintf
         "Expected both unused_a and unused_b, got unused_a=%b, unused_b=%b"
         has_unused_a has_unused_b)

(* Test 6: --fix functionality *)
let test_fix_functionality () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "unused.ml" (Filename.concat tmp_dir "unused.ml");
  let _ = run_analyzer ~fix:true tmp_dir in
  let ic = open_in_bin (Filename.concat tmp_dir "unused.ml") in
  let content =
    really_input_string ic
      (Unix.stat (Filename.concat tmp_dir "unused.ml")).st_size
  in
  close_in ic;
  if not (string_contains content "unused_function") then ()
  else failwith "Expected unused_function to be removed"

(* Test 7: Public function used should not be reported *)
let test_public_used () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "unused.ml" (Filename.concat tmp_dir "unused.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  (* Check that "used_function" is not reported as unused *)
  if not (string_contains output "Unused function 'used_function'") then ()
  else failwith "Used function should not be reported"

(* Test 8: Empty directory *)
let test_empty_directory () =
  with_temp_dir @@ fun tmp_dir ->
  let result, output = run_analyzer ~fix:false tmp_dir in
  if result = 0 then
    if string_contains output "No OCaml files found" then ()
    else failwith "Expected message about no OCaml files"
  else failwith "Expected exit code 0 for empty directory"

(* Test 9: AST coverage - various OCaml constructs *)
let test_ast_coverage () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "ast_coverage.ml" (Filename.concat tmp_dir "ast_coverage.ml");
  let result, output = run_analyzer ~fix:false tmp_dir in
  if result = 1 then
    (* Should find some unused functions *)
    if string_contains output "Unused function" then ()
    else failwith "Expected to find some unused functions"
  else failwith "Expected exit code 1 for ast_coverage test"

(* Test 10: Pattern matching - functions used in match expressions *)
let test_pattern_matching () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "pattern_matching.ml"
    (Filename.concat tmp_dir "pattern_matching.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  (* check_positive, double, get_default should NOT be reported *)
  if string_contains output "check_positive" then
    failwith "check_positive should not be reported (used in guard)"
  else if string_contains output "double" then
    failwith "double should not be reported (used in match)"
  else if string_contains output "get_default" then
    failwith "get_default should not be reported (used in match)"
  else ()

(* Test 11: Records - functions used in record field expressions *)
let test_records () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "records.ml" (Filename.concat tmp_dir "records.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  (* transform_x, extract_y, increment should NOT be reported *)
  if string_contains output "transform_x" then
    failwith "transform_x should not be reported (used in record field)"
  else if string_contains output "extract_y" then
    failwith "extract_y should not be reported (used in pattern)"
  else if string_contains output "increment" then
    failwith "increment should not be reported (used in record update)"
  else ()

(* Test 12: Higher-order functions - functions passed as arguments *)
let test_higher_order () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "higher_order.ml" (Filename.concat tmp_dir "higher_order.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  (* add_five, summer, is_positive, square should NOT be reported *)
  if string_contains output "add_five" then
    failwith "add_five should not be reported (passed to map)"
  else if string_contains output "summer" then
    failwith "summer should not be reported (passed to fold)"
  else if string_contains output "is_positive" then
    failwith "is_positive should not be reported (passed to filter)"
  else if string_contains output "square" then
    failwith "square should not be reported (passed as argument)"
  else ()

(* Test 13: Comprehensive AST - various OCaml language features *)
(* Tests that functions called within various syntax constructs are tracked *)
let test_comprehensive_ast () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "comprehensive_ast.ml"
    (Filename.concat tmp_dir "comprehensive_ast.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  (* Helper functions called within various constructs should NOT be reported *)
  if string_contains output "helper_for_local" then
    failwith "helper_for_local should not be reported (used in local module)"
  else if string_contains output "functor_helper" then
    failwith "functor_helper should not be reported (used in functor)"
  else if string_contains output "class_helper" then
    failwith "class_helper should not be reported (used in class method)"
  else if string_contains output "handle_error" then
    failwith "handle_error should not be reported (used in try-with)"
  else if string_contains output "object_helper" then
    failwith "object_helper should not be reported (used in object expression)"
  else if string_contains output "let_open_helper" then
    failwith "let_open_helper should not be reported (used in let-open)"
  else if string_contains output "loop_helper" then
    failwith "loop_helper should not be reported (used in loop)"
  else if string_contains output "record_x" then
    failwith "record_x should not be reported (used in record)"
  else if string_contains output "record_y" then
    failwith "record_y should not be reported (used in record)"
  else if string_contains output "array_helper" then
    failwith "array_helper should not be reported (used in array)"
  else if string_contains output "list_helper" then
    failwith "list_helper should not be reported (used in list)"
  else if string_contains output "option_helper" then
    failwith "option_helper should not be reported (used in option)"
  else if string_contains output "lazy_helper" then
    failwith "lazy_helper should not be reported (used in lazy)"
  else if string_contains output "compose_helper" then
    failwith "compose_helper should not be reported (used in compose)"
  else if string_contains output "rec_helper" then
    failwith "rec_helper should not be reported (used in recursive)"
  else if string_contains output "mut_helper" then
    failwith "mut_helper should not be reported (used in mutual recursion)"
  else if string_contains output "seq_helper" then
    failwith "seq_helper should not be reported (used in sequence)"
  else if string_contains output "if_helper" then
    failwith "if_helper should not be reported (used in if-then-else)"
  else if string_contains output "assert_helper" then
    failwith "assert_helper should not be reported (used in assert)"
    (* unused_function should be reported as unused *)
  else if string_contains output "Unused function 'unused_function'" then ()
  else failwith "Expected unused_function to be reported as unused"

(* Test 14: Effect handlers - OCaml 5+ effect handler syntax *)
(* Tests that functions called in effect handler bodies are tracked *)
let test_effect_handlers () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "effect_handlers.ml" (Filename.concat tmp_dir "effect_handlers.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  (* Functions called in effect handler bodies should NOT be reported as unused *)
  if string_contains output "log_message" then
    failwith "log_message should not be reported (called in handler body)"
  else if string_contains output "get_current_state" then
    failwith "get_current_state should not be reported (called in handler body)"
  else if string_contains output "update_state" then
    failwith "update_state should not be reported (called in handler body)"
  else if string_contains output "handle_both_choices" then
    failwith
      "handle_both_choices should not be reported (called in handler body)"
  else if string_contains output "format_echo_msg" then
    failwith "format_echo_msg should not be reported (called in handler body)"
    (* unused_function should be reported as unused *)
  else if string_contains output "Unused function 'unused_function'" then ()
  else failwith "Expected unused_function to be reported as unused"

(* Test 15: All AST nodes - comprehensive coverage of Parsetree *)
(* Tests that all major AST node types can be parsed without error *)
let test_all_ast_nodes () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "all_ast_nodes.ml" (Filename.concat tmp_dir "all_ast_nodes.ml");
  (* Should parse without error - just verify no crash *)
  let config = Dzarn.Config.default in
  let _code = Dzarn.Analyzer.run ~fix:false ~config tmp_dir in
  (* Exit code may be non-zero if there are unused functions, but that's OK *)
  (* We just want to ensure parsing succeeded *)
  ()

(* Test 16: Qualified module calls - Module.function syntax *)
(* Bug: qualified calls like A.func incorrectly mark top-level 'func' as used *)
let test_qualified_module_call () =
  with_temp_dir @@ fun tmp_dir ->
  copy_file "qualified_module_call.ml"
    (Filename.concat tmp_dir "qualified_module_call.ml");
  let _, output = run_analyzer ~fix:false tmp_dir in
  (* The top-level 'record' and 'helper' should be reported as unused *)
  (* because A.record and B.helper are calls to local modules, not top-level *)
  if string_contains output "Unused function 'record'" then
    if string_contains output "Unused function 'helper'" then ()
    else failwith "helper should be reported as unused"
  else failwith "record should be reported as unused"

let () =
  let open Alcotest in
  run "dzarn"
    [
      ( "Detect unused function",
        [ test_case "basic detection" `Quick test_detect_unused ] );
      ( "Private functions",
        [ test_case "underscore prefix" `Quick test_private_functions ] );
      ( "Mli declared",
        [ test_case ".mli declarations" `Quick test_mli_declared ] );
      ("All used", [ test_case "no unused functions" `Quick test_all_used ]);
      ("Multi-file", [ test_case "cross-file detection" `Quick test_multi_file ]);
      ( "Fix functionality",
        [ test_case "--fix removes functions" `Quick test_fix_functionality ] );
      ( "Public used",
        [ test_case "used function not reported" `Quick test_public_used ] );
      ( "Empty directory",
        [ test_case "handles empty dir" `Quick test_empty_directory ] );
      ( "AST coverage",
        [ test_case "various OCaml constructs" `Quick test_ast_coverage ] );
      ( "Pattern matching",
        [
          test_case "functions in match expressions" `Quick
            test_pattern_matching;
        ] );
      ( "Records",
        [ test_case "functions in record contexts" `Quick test_records ] );
      ( "Higher-order functions",
        [ test_case "functions passed as arguments" `Quick test_higher_order ]
      );
      ( "Comprehensive AST",
        [ test_case "various OCaml features" `Quick test_comprehensive_ast ] );
      ( "Effect handlers",
        [ test_case "OCaml 5+ effect handlers" `Quick test_effect_handlers ] );
      ( "All AST nodes",
        [ test_case "complete Parsetree coverage" `Quick test_all_ast_nodes ] );
      ( "Qualified module calls",
        [ test_case "Module.function syntax" `Quick test_qualified_module_call ]
      );
    ]
