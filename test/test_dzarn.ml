open Dzarn

let test_run_dummy () =
  Main.run "";
  ()

let () =
  let open Alcotest in
  run "dzarn" [ ("Main.run", [ test_case "dummy" `Quick test_run_dummy ]) ]
