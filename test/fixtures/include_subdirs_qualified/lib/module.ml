let hello () = "hello"
let unused () = ()
let greet () = hello () ^ " " ^ Subdir.Inner.world ()
