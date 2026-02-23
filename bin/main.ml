open Dzarn

let main () =
  let open Cmdliner in
  Cmd.(
    v (info "dzarn")
      Term.(
        const Main.run
        $ Arg.(required & pos 0 (some string) None & info ~docv:"DIR" [])))
  |> Cmd.eval ~catch:false

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  main () |> exit
