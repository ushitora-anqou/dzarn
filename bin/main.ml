open Cmdliner

let fix =
  let doc = "Remove unused functions automatically" in
  Arg.(value & flag & info [ "fix" ] ~doc)

let config_file =
  let doc = "Path to configuration file (sexp format)" in
  Arg.(
    value & opt string "dzarn.sexp" & info [ "c"; "config" ] ~docv:"FILE" ~doc)

let run dir fix config_path =
  (* Load config or use default *)
  let config =
    if Sys.file_exists config_path then (
      try Dzarn.Config.parse_file config_path
      with e ->
        Printf.eprintf "Warning: Failed to load config file '%s': %s\n"
          config_path (Printexc.to_string e);
        Dzarn.Config.default)
    else Dzarn.Config.default
  in
  Dzarn.Analyzer.run ~fix ~config dir

let main () =
  Cmd.(
    v (info "dzarn")
      Term.(
        const (fun dir fix config_path -> exit (run dir fix config_path))
        $ Arg.(required & pos 0 (some string) None & info ~docv:"DIR" [])
        $ fix $ config_file))
  |> Cmd.eval ~catch:false

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  ignore (main ())
