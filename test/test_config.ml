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
  else failwith "Default: naming_enabled should be true"

(* Test 2: Parse full config *)
let test_parse_full () =
  let config =
    Config.parse_string
      "((unused_enabled false) (complexity_enabled true) (complexity_threshold \
       15) (naming_enabled false))"
  in
  if config.Config.unused_enabled then
    failwith "Full: unused_enabled should be false"
  else if not config.Config.complexity_enabled then
    failwith "Full: complexity_enabled should be true"
  else if config.Config.complexity_threshold <> 15 then
    failwith "Full: complexity_threshold should be 15"
  else if config.Config.naming_enabled then
    failwith "Full: naming_enabled should be false"
  else ()

(* Test 3: Parse with comments *)
let test_parse_with_comments () =
  let config =
    Config.parse_string
      "; This is a comment\n\
       ((unused_enabled true) (complexity_enabled true) (complexity_threshold \
       10) (naming_enabled true))"
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
