open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts
open Ctypes
open Foreign


let _ = Llvm_executionengine.initialize ()
let compiler_options = {
  Llvm_executionengine.default_compiler_options with
  opt_level = 3 }

(* Create the JIT. *)
let exec the_module =
(* Optimizations are not working for some reason... *)
(*
  let the_fpm = PassManager.create_function the_module in

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_memory_to_register_promotion the_fpm;

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  add_instruction_combination the_fpm;

  (* reassociate expressions. *)
  add_reassociation the_fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_memory_to_register_promotion the_fpm;

  ignore (PassManager.initialize the_fpm);
*)

  let the_execution_engine = Llvm_executionengine.create the_module ~options:compiler_options in

  let ct = funptr ( void @-> returning int ) in
  let func = Llvm_executionengine.get_function_address "main::anon" ct the_execution_engine in

  (* Print out all the generated code. *)
  dump_module the_module;

  let res = func () in
  Printf.printf "Result: %d\n" res
