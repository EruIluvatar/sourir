open Llvm
open Instr

exception Error of string

let context = global_context ()
let builder = builder context
let i32 = i32_type context

let func_decl the_module name formals =
  let params = Array.make (List.length formals) i32 in
  let ft = function_type i32 params in
  match lookup_function name the_module with
  | None -> declare_function name ft the_module
  | Some _ -> assert false

let func_lookup the_module name =
  match lookup_function name the_module with
  | Some f -> f
  | None -> assert false

(* for each function, find the first version *)
let get_active_version the_module program fun_name =
  let rec iter_functions = function
    | func :: functions ->
        if func.name = fun_name then
          (*find first version *)
          let active_version = List.hd func.body in
          (* return it *)
          (String.concat "::" [fun_name; active_version.label])
        else
          (* iterate again *)
          iter_functions functions
    | [ ] ->
        assert(false)
  in
  let name = iter_functions (program.main :: program.functions) in
  func_lookup the_module name

let generate_func the_module func program scope formals (prog : instructions) : unit =
  (* vars remembers the declaration of local variables
   * labels remembers the bb's of labels
   * We use the infered declaration site of the variable for the index. *)
  let vars : (instr_position * variable, llvalue) Hashtbl.t = Hashtbl.create 10 in
  let labels : (string, llbasicblock) Hashtbl.t = Hashtbl.create 10 in
  let lookup_var id = (try Hashtbl.find vars id with
                       | Not_found -> raise (Error "unknown variable name")) in
  let lookup_label l = (try Hashtbl.find labels l with
                       | Not_found -> raise (Error "unknown label name")) in

  (* create entry block for function *)
  let bb = append_block context "entry" func in
  position_at_end bb builder;

  (* Store function arguments into the vars table,
   * set the names of the (positional) function arguments *)
  List.iteri (fun i (Param name) ->
    let arg = (params func).(i) in
    let id = (Arg, name) in
    set_value_name name arg;
    Hashtbl.add vars id arg;
  ) formals;

  (* Pass 1: Generate Basic blocks and declare local variables *)
  let dump_instr func pc instr : unit =
    let var_id var = (scope pc var, var) in
    let llvm_value = function
      | Int i -> const_int i32 i
      | Fun_ref f -> get_active_version the_module program f
      | (Nil|Bool _|Array _) -> assert(false)
    in
    let simple = function
      | Var v             ->
          let id = var_id v in
          begin match id with
          | Arg, x     ->
              let arg = lookup_var id in
              arg
          | Instr i, x ->
              let alloca = lookup_var id in
              build_load alloca x builder
          end
      | Constant c        -> llvm_value c
    in
    let dump_expr exp : Llvm.llvalue =
      match exp with
      | Simple e           -> simple e
      | Unop (Neg, a)      -> simple a
      | Unop (Not, a)      -> simple a
      | Binop (Plus, a, b) -> build_add  (simple a) (simple b) "addtmp" builder
      | Binop (Sub,  a, b) -> build_sub  (simple a) (simple b) "subtmp" builder
      | Binop (Mult, a, b) -> build_mul  (simple a) (simple b) "multtmp" builder
      | Binop (Div,  a, b) -> build_udiv (simple a) (simple b) "divtmp" builder
      | Binop (Eq,   a, b) -> build_icmp Icmp.Eq  (simple a) (simple b) "eqtmp" builder
      | Binop (Neq,  a, b) -> build_icmp Icmp.Ne  (simple a) (simple b) "neqtmp" builder
      | Binop (Lt,   a, b) -> build_icmp Icmp.Slt (simple a) (simple b) "lttmp" builder
      | Binop (Lte,  a, b) -> build_icmp Icmp.Sle (simple a) (simple b) "ltetmp" builder
      | Binop (Gt,   a, b) -> build_icmp Icmp.Sgt (simple a) (simple b) "gttmp" builder
      | Binop (Gte,  a, b) -> build_icmp Icmp.Sge (simple a) (simple b) "gtetmp" builder
      | Binop (And,  _, _)
      | Binop (Or,   _, _)
      | Binop (Mod,  _, _)
      | Array_index (_, _)
      | Array_length _     -> assert(false)
    in

    begin match instr with
    | Decl_var (var, exp) ->
        let alloca = build_alloca i32 var builder in
        let id = (Instr pc, var) in
        Hashtbl.add vars id alloca;
        ()
    | Call (l, var, f, args) ->
        (* Call is not an expression, it stores the return value in l.
         * Therefor we need an alloca *)
        let alloca = build_alloca i32 var builder in
        let id = (Instr pc, var) in
        Hashtbl.add vars id alloca;
        ()
    | Decl_array (var, Length exp) ->
        let alloca = build_array_alloca i32 (dump_expr exp) var builder in
        let id = (Instr pc, var) in
        Hashtbl.add vars id alloca;
        ()
    | Decl_array (var, List li) ->
        let alloca = build_array_alloca i32 (const_int i32 (List.length li)) var builder in
        let id = (Instr pc, var) in
        Hashtbl.add vars id alloca;
        ()
    | Label (MergeLabel label | BranchLabel label) ->
        (* Create own basic blocks, stored in 'labels' table *)
        let bb = append_block context label func in
        Hashtbl.add labels label bb
    | _ ->
        ()
    end
  in
  Array.iteri (dump_instr func) prog;

  (* Pass 2: Compile instructions *)
  let dump_instr func pc instr : unit =
    let var_id var = (scope pc var, var) in
    let llvm_value = function
      | Int i -> const_int i32 i
      | Fun_ref f -> get_active_version the_module program f
      | (Nil|Bool _|Array _) -> assert(false)
    in
    let simple = function
      | Var v             ->
          let id = var_id v in
          begin match id with
          | Arg, x     ->
              let arg = lookup_var id in
              arg
          | Instr i, x ->
              let alloca = lookup_var id in
              build_load alloca x builder
          end
      | Constant c        -> llvm_value c
    in
    let dump_expr exp : Llvm.llvalue =
      match exp with
      | Simple e           -> simple e
      | Unop (Neg, a)      -> simple a
      | Unop (Not, a)      -> simple a
      | Binop (Plus, a, b) -> build_add  (simple a) (simple b) "addtmp" builder
      | Binop (Sub,  a, b) -> build_sub  (simple a) (simple b) "subtmp" builder
      | Binop (Mult, a, b) -> build_mul  (simple a) (simple b) "multtmp" builder
      | Binop (Div,  a, b) -> build_udiv (simple a) (simple b) "divtmp" builder
      | Binop (Eq,   a, b) -> build_icmp Icmp.Eq  (simple a) (simple b) "eqtmp" builder
      | Binop (Neq,  a, b) -> build_icmp Icmp.Ne  (simple a) (simple b) "neqtmp" builder
      | Binop (Lt,   a, b) -> build_icmp Icmp.Slt (simple a) (simple b) "lttmp" builder
      | Binop (Lte,  a, b) -> build_icmp Icmp.Sle (simple a) (simple b) "ltetmp" builder
      | Binop (Gt,   a, b) -> build_icmp Icmp.Sgt (simple a) (simple b) "gttmp" builder
      | Binop (Gte,  a, b) -> build_icmp Icmp.Sge (simple a) (simple b) "gtetmp" builder
      | Binop (And,  _, _)
      | Binop (Or,   _, _)
      | Binop (Mod,  _, _)
      | Array_index (_, _)
      | Array_length _     -> assert(false)
    in

    let dump_arg arg = dump_expr arg in
    begin match instr with
    | Return exp ->
        let ret_val = dump_expr exp in
        build_ret ret_val builder;
        ()
    | Assign (var, exp)
    | Decl_var (var, exp) ->
        let start_val = dump_expr exp in
        let id = (scope (pc+1) var, var) in
        let alloca = lookup_var id in
        (* Store value into alloc *)
        ignore(build_store start_val alloca builder);
        ()
    | Call (l, var, f, args) ->
        (* get function reference `'foo ()` *)
        let func_ref = dump_expr f in
        (* and its arguments (casted to Array)*)
        let func_args = List.map dump_expr args in
        let func_args = Array.of_list func_args in
        (* build the call, get return value *)
        let ret_val = build_call func_ref func_args "calltmp" builder in

        (* lookup the variable name  *)
        let id = (Instr pc, var) in
        let alloca = lookup_var id in
        (* Store value into alloc *)
        ignore(build_store ret_val alloca builder);
        ()
    | Stop exp ->
        assert(false)
    | Decl_array (var, Length exp) ->
        ()
    | Decl_array (var, List li) ->
        ()
    | Drop var ->
        assert(false)
    | Array_assign (var, index, exp) ->
        assert(false)
    | Branch (exp, l1, l2) ->
        (* add basic block to builder at current position *)
        let l1 = lookup_label l1 in
        let l2 = lookup_label l2 in
        build_cond_br (dump_expr exp) l1 l2 builder; ()
    | Label (MergeLabel label | BranchLabel label) ->
        (* add basic block to builder at current position *)
        let bb = lookup_label label in
        position_at_end bb builder; ()
    | Goto label ->
        let bb = lookup_label label in
        build_br bb builder; ()
    | Print exp ->
        ()
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

(* entry point *)
let generate (program : Instr.program) =
  let the_module = create_module context "Sourir LLVM Jit" in
  let open Types in
  (* Pass 1: declare functions *)
  List.iter (fun ({name; formals; body} as sourir_function) ->
    List.iter (fun version ->
      let llvm_function = func_decl the_module (String.concat "::" [name; version.label]) formals in
      ()
    ) body
  ) (program.main :: program.functions);

  (* Pass 2: lookup current function, generate its content *)
  List.iter (fun ({name; formals; body} as sourir_function) ->
    List.iter (fun version ->
      let llvm_function = func_lookup the_module (String.concat "::" [name; version.label]) in
      let scope = Scope.infer_decl (Analysis.as_analysis_input sourir_function version) in
      generate_func the_module llvm_function program scope formals version.instrs;
      Llvm_analysis.assert_valid_function llvm_function) body
    ) (program.main :: program.functions);
  the_module
