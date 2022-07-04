(*
 * Emulation of (a subset of) the `env` module currently used by Binaryen,
 * so that we can run modules generated by Binaryen. This is a stopgap until
 * we have agreement on what libc should look like.
 *)

open Values
open Types
open Instance

let error msg = raise (Eval.Crash (Source.no_region, msg))

let type_error v t =
  error
    ("type error, expected " ^ string_of_value_type t ^ ", got "
    ^ string_of_value_type (type_of_value v))

let empty = function [] -> () | vs -> error "type error, too many arguments"

let single = function
  | [] -> error "type error, missing arguments"
  | [v] -> v
  | vs -> error "type error, too many arguments"

let int = function
  | Num (I32 i) -> Int32.to_int i
  | v -> type_error v (NumType I32Type)

let abort vs =
  empty vs ;
  print_endline "Abort!" ;
  exit (-1)

let exit vs = exit (int (single vs))

let lookup name t =
  match (Utf8.encode name, t) with
  | "abort", ExternFuncType t -> ExternFunc (Func.alloc_host t abort)
  | "exit", ExternFuncType t -> ExternFunc (Func.alloc_host t exit)
  | _ -> raise Not_found
