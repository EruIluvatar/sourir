open Llvm
open Instr

exception Error of string

let context = global_context ()
let the_module = create_module context "Sourir LLVM Jit"
let builder = builder context
let i32 = i32_type context

let create_entry_block_alloca the_function var_name =
  let builder = builder_at context (instr_begin (entry_block the_function)) in
  build_alloca i32 var_name builder

let func_decl name =
  (* Make the function type: double(double,double) etc. *)
  let ft = function_type i32 [| |] in
  match lookup_function name the_module with
  | None -> declare_function name ft the_module
  | Some _ -> assert false

let generate_instr func scope formals (prog : instructions) : unit =
  (* the llvm_scope remembers the declaration of local variables. We use
   * the infered declaration site of the variable for the index. *)
  let llvm_scope : (instr_position * variable, llvalue) Hashtbl.t = Hashtbl.create 10 in

  let bb = append_block context "entry" func in
  position_at_end bb builder;

  (* todo : function arguments *)

  let dump_instr func pc instr : unit =
    let var_id var = (scope pc var, var) in
    let value_ = function
      | Int i -> const_int i32 i
      | (Nil|Bool _|Fun_ref _|Array _) -> assert(false)
    in
    let simple = function
      | Var v             ->
          let id = var_id v in
          begin match id with
          | Arg, x     -> Printf.printf "Variable %s on line %d is a function argument\n" x pc
          | Instr i, x -> Printf.printf "Variable %s on line %d is declared at %d\n" x pc i
          end ;
          (try Hashtbl.find llvm_scope id with
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
      | Binop (Mod,  _, _)
      | Binop (Eq,   _, _)
      | Binop (Neq,  _, _)
      | Binop (Lt,   _, _)
      | Binop (Lte,  _, _)
      | Binop (Gt,   _, _)
      | Binop (Gte,  _, _)
      | Binop (And,  _, _)
      | Binop (Or,   _, _)
      | Array_index (_, _)
      | Array_length _     -> assert(false)
    in
    let dump_arg arg = dump_expr arg in
    begin match instr with
    | Return exp                      ->
        Printf.printf "ret\n";
        let ret_val = dump_expr exp in
        build_ret ret_val builder;
        ()
    | Decl_var (var, exp) ->
        let start_val = dump_expr exp in
        (* let start_val = const_int i32 0 in *)

        (*let alloca = build_alloca i32 var bb in *)
        let alloca = create_entry_block_alloca func var in
        (* Store value into alloc *)
        ignore(build_store start_val alloca builder);
        let id = (Instr pc, var) in
        Hashtbl.add llvm_scope id alloca;
        ()
    | Call (l, var, f, args) ->
        assert(false)
    | Stop exp ->
        assert(false)
    | Decl_array (var, Length exp) ->
        assert(false)
    | Decl_array (var, List li) ->
        assert(false)
    | Drop var ->
        assert(false)
    | Assign (var, exp) ->
        assert(false)
    | Array_assign (var, index, exp) ->
        assert(false)
    | Branch (exp, l1, l2) ->
        assert(false)
    | Label (MergeLabel label) ->
        assert(false)
    | Label (BranchLabel label) ->
        assert(false)
    | Goto label ->
        assert(false)
    | Print exp ->
        assert(false)
    | Assert exp ->
        assert(false)
    | Guard_hint es ->
        assert(false)
    | Read var ->
        assert(false)
    | Assume {label; guards; target={func; version; pos}; varmap; extra_frames} ->
        assert(false)
    | Comment str ->
        ()
    end
  in
  Array.iteri (dump_instr func) prog

let generate (program : Instr.program) =
  let open Types in
  List.iter (fun ({name; formals; body} as sourir_function) ->
      List.iter (fun version ->
        let llvm_function = func_decl (String.concat "::" [name; version.label]) in
        let scope = Scope.infer_decl (Analysis.as_analysis_input sourir_function version) in
        generate_instr llvm_function scope formals version.instrs) body
    ) (program.main :: program.functions);
  dump_module the_module

