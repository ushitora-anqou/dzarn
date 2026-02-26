(* Config file parser tests *)

open Stdlib
open Dzarn

(* Test 1: Default config *)
let test_default_config () =
  let default = Config.default in
  if default.Config.unused_enabled then ()
  else failwith "Default: unused_enabled should be true";
  if default.Config.complexity_enabled then ()
  else failwith "Default: complexity_enabled should be true";
  if default.Config.complexity_threshold = 10 then ()
  else failwith "Default: complexity_threshold should be 10";
  if default.Config.naming_enabled then ()
  else failwith "Default: naming_enabled should be true";
  if default.Config.length_enabled then ()
  else failwith "Default: length_enabled should be true";
  if default.Config.length_threshold = 50 then ()
  else failwith "Default: length_threshold should be 50";
  if default.Config.unused_nolint_enabled then ()
  else failwith "Default: unused_nolint_enabled should be true";
  if not default.Config.json_output then ()
  else failwith "Default: json_output should be false"

(* Test 2: Parse full config *)
let test_parse_full () =
  let config =
    Config.parse_string
      "((unused_enabled false) (complexity_enabled true) (complexity_threshold \
       15) (naming_enabled false) (length_enabled true) (length_threshold 100) \
       (unused_nolint_enabled false) (json_output true))"
  in
  if config.Config.unused_enabled then
    failwith "Full: unused_enabled should be false"
  else if not config.Config.complexity_enabled then
    failwith "Full: complexity_enabled should be true"
  else if config.Config.complexity_threshold <> 15 then
    failwith "Full: complexity_threshold should be 15"
  else if config.Config.naming_enabled then
    failwith "Full: naming_enabled should be false"
  else if not config.Config.length_enabled then
    failwith "Full: length_enabled should be true"
  else if config.Config.length_threshold <> 100 then
    failwith "Full: length_threshold should be 100"
  else if config.Config.unused_nolint_enabled then
    failwith "Full: unused_nolint_enabled should be false"
  else if not config.Config.json_output then
    failwith "Full: json_output should be true"
  else ()

(* Test 3: Parse with comments *)
let test_parse_with_comments () =
  let config =
    Config.parse_string
      "; This is a comment\n\
       ((unused_enabled true) (complexity_enabled true) (complexity_threshold \
       10) (naming_enabled true) (length_enabled true) (length_threshold 50) \
       (unused_nolint_enabled true) (json_output false))"
  in
  if config.Config.unused_enabled then ()
  else failwith "Comments: unused_enabled should be true"

let () =
  let open Alcotest in
  run "Config"
    [
      ( "Default config",
        [ test_case "default values" `Quick test_default_config ] );
      ("Parse full", [ test_case "full config" `Quick test_parse_full ]);
      ( "Parse with comments",
        [ test_case "comments ignored" `Quick test_parse_with_comments ] );
    ]
