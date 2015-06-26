open Bap.Std
(** {4 Term type classes}  *)
(* {BEGIN: classes} *)
val sub_t : (program, sub) cls (** sub  *)
val arg_t : (sub, arg) cls     (** arg  *)
val blk_t : (sub, blk) cls     (** blk  *)
val phi_t : (blk, phi) cls     (** phi  *)
val def_t : (blk, def) cls     (** def  *)
val jmp_t : (blk, jmp) cls     (** jmp  *)
(* {END} *)

