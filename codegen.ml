open Llvm
open Instr

exception Error of string

let context = global_context ()
let the_module = create_module context "Sourir LLVM Jit"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context


let generate_instr (prog : instructions) =
  let dump_instr pc instr =
    let simple = function
      | Var v             -> (try Hashtbl.find named_values v with
        | Not_found -> raise (Error "unknown variable name")
      | Constant c        -> const_int i32_type c
    in
    let dump_expr exp =
      match exp with
      | Simple e           -> simple e
      | Unop (Neg, a)      -> simple a
      | Unop (Not, a)      -> simple a
      | Binop (Plus, a, b) -> build_add (simple a) (simple b) "addtmp" builder
      | Binop (Sub, a, b)  -> build_sub (simple a) (simple b) "subtmp" builder
      | Binop (Mult, a, b) -> build_mult (simple a) (simple b) "multtmp" builder
      | Binop (Div,  a, b) -> build_div (simple a) (simple b) "divtmp" builder
      | Binop (Mod,  a, b) -> simple a simple b
      | Binop (Eq,   a, b) -> simple a simple b
      | Binop (Neq,  a, b) -> simple a simple b
      | Binop (Lt,   a, b) -> simple a simple b
      | Binop (Lte,  a, b) -> simple a simple b
      | Binop (Gt,   a, b) -> simple a simple b
      | Binop (Gte,  a, b) -> simple a simple b
      | Binop (And,  a, b) -> simple a simple b
      | Binop (Or,   a, b) -> simple a simple b
      | Array_index (a, i) -> a simple i
      | Array_length e     -> simple e
    in
    let dump_arg arg = dump_expr arg in
    format_pc pc;
    begin match instr with
    | Call (l, var, f, args)          ->
      pr " call %s = "var;
      dump_expr f;
      (dump_comma_separated dump_arg) args l;
    | Stop exp                        -> dump_expr exp
    | Return exp                      -> dump_expr exp
    | Decl_var (var, exp)             -> var dump_expr exp
    | Decl_array (var, Length exp)    -> var dump_expr exp
    | Decl_array (var, List li)       -> var
                                           (dump_comma_separated dump_expr) li
    | Drop var                        -> var
    | Assign (var, exp)               -> var dump_expr exp
    | Array_assign (var, index, exp)  -> var dump_expr index dump_expr exp
    | Branch (exp, l1, l2)            -> dump_expr exp l1 l2
    | Label (MergeLabel label)        -> label
    | Label (BranchLabel label)       -> label
    | Goto label                      -> label
    | Print exp                       -> dump_expr exp
    | Assert exp                      -> dump_expr exp
    | Guard_hint es                   -> (dump_comma_separated dump_expr) es
    | Read var                        -> var
    | Assume {label; guards; target={func; version; pos}; varmap; extra_frames} ->
      let dump_var = function
        | x, e -> x dump_expr e
      in
      let dump_frame {cont_pos={func; version; pos}; cont_res; varmap} =
        pr "(%s, %s, %s) [var %s = $%s%a]"
            func version pos
            cont_res
            (if varmap = [] then "" else ", ")
            (dump_comma_separated dump_var) varmap
      in
      pr " assume %s [%a] else (%s, %s, %s) [%a]%s%a"
        label
        (dump_comma_separated dump_expr) guards
        func version pos
        (dump_comma_separated dump_var) varmap
        (if extra_frames = [] then "" else ", ")
        (dump_comma_separated dump_frame) extra_frames
    | Comment str                     -> str
    end;
    pr "\n"
  in
  Array.iteri (dump_instr) prog

let generate (program : Instr.program) =
    generate_instr program.main;
    dump_module the_module
  (*List.iter (fun {name; formals; body} ->
      List.iter (fun version ->
          version.label;
          generate_instr version.instrs) body
    ) (program.main :: program.functions)
    *)

