open Bar

let f () = ()

let () =
  let* x = f () |> hoge in
  Ok ()
