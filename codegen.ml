open Llvm
open Instr

exception Error of string

let context = global_context ()
let the_module = create_module context "Sourir LLVM Jit"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let i32 = i32_type context

let func_decl name =
  (* Make the function type: double(double,double) etc. *)
  let ft = function_type i32 [| |] in
  match lookup_function name the_module with
  | None -> declare_function name ft the_module
  | Some _ -> assert false

let generate_instr func scope (prog : instructions) : unit =
  let bb = append_block context "entry" func in
  position_at_end bb builder;

  let dump_instr func pc instr : unit =
    let value_ = function 
      | Int i -> const_int i32 i
    in
    let simple = function
      | Var v             -> (try Hashtbl.find named_values v with
        | Not_found -> raise (Error "unknown variable name"))
      | Constant c        -> value_ c
    in
    let dump_expr exp : Llvm.llvalue =
      match exp with
      | Simple e           -> simple e
      | Unop (Neg, a)      -> simple a
      | Unop (Not, a)      -> simple a
      | Binop (Plus, a, b) -> build_add (simple a) (simple b) "addtmp" builder
      | Binop (Sub, a, b)  -> build_sub (simple a) (simple b) "subtmp" builder
      | Binop (Mult, a, b) -> build_mul (simple a) (simple b) "multtmp" builder
      | Binop (Div,  a, b) -> build_udiv (simple a) (simple b) "divtmp" builder
      (*| Binop (Mod,  a, b) -> simple a simple b
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
      *)
    in
    let dump_arg arg = dump_expr arg in
    let _ = begin match instr with
    (*| Call (l, var, f, args)          ->
      pr " call %s = "var;
      dump_expr f;
      (dump_comma_separated dump_arg) args l;
    | Stop exp                        -> dump_expr exp
    *)
    | Return exp                      -> 
        let ret_val = dump_expr exp in
        build_ret ret_val builder
    (*| Decl_var (var, exp)             -> var dump_expr exp
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
    *)
    end in ()
  in
  Array.iteri (dump_instr func) prog

let generate (program : Instr.program) =
  List.iter (fun {name; formals; body} ->
      List.iter (fun version ->
        let func = func_decl (String.concat "::" [name; version.label]) in
        let scope = Analysis.lifetime_analysis {instrs = version.instrs; formals} in
        generate_instr func scope version.instrs) body
    ) (program.main :: program.functions);
  dump_module the_module

