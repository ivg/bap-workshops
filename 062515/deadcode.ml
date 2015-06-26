(* {BEGIN: start} *)
open Core_kernel.Std
open Bap.Std

let main p = p

let () = Project.register_pass ~deps:["ssa"] "deadcode" main
(* {END} *)
