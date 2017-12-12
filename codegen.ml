open Llvm
open Instr

exception Error of string

let context = global_context ()
let builder = builder context
let i32 = i32_type context

let create_entry_block_alloca the_function var_name =
  let builder = builder_at context (instr_begin (entry_block the_function)) in
  build_alloca i32 var_name builder

let func_decl the_module name formals =
  (* Make the function type: double(double,double) etc. *)
  let params = Array.make (List.length formals) i32 in
  let ft = function_type i32 params in
  match lookup_function name the_module with
  | None -> declare_function name ft the_module
  | Some _ -> assert false

let func_lookup the_module name =
  (* Make the function type: double(double,double) etc. *)
  match lookup_function name the_module with
  | Some f -> f
  | None -> assert false

let get_active_version the_module program (fun_name : string)=
  let rec iter_functions = function
    | func :: functions ->
        if func.name = fun_name then
          (*find first version *)
          let active_version = List.hd func.body in
          (String.concat "::" [fun_name; active_version.label])
        else
          iter_functions functions
    | [ ] ->
        assert(false)
  in
  let name = iter_functions (program.main :: program.functions) in
  func_lookup the_module name


let generate_instr program the_module func scope formals (prog : instructions) : unit =
  (* the llvm_scope remembers the declaration of local variables. We use
   * the infered declaration site of the variable for the index. *)
  let llvm_scope : (instr_position * variable, llvalue) Hashtbl.t = Hashtbl.create 10 in
  let labels : (string, llbasicblock) Hashtbl.t = Hashtbl.create 10 in

  let bb = append_block context "entry" func in
  position_at_end bb builder;

  (* todo : function arguments *)
  List.iteri (fun i (Param name) ->
    let arg = (params func).(i) in
    let id = (Arg, name) in
    set_value_name name arg;
    Hashtbl.add llvm_scope id arg;
  ) formals;

  (* Pass 1: Generate Basic blocks and declare local variables *)
  let dump_instr func pc instr : unit =
    begin match instr with
    | Decl_var (var, exp) ->
        let alloca = build_alloca i32 var builder in
        let id = (Instr pc, var) in
        Hashtbl.add llvm_scope id alloca;
        ()
    | Call (l, var, f, args) ->
        let alloca = build_alloca i32 var builder in
        let id = (Instr pc, var) in
        Hashtbl.add llvm_scope id alloca;
        ()
    | Decl_array (var, Length exp) ->
        assert(false)
    | Decl_array (var, List li) ->
        assert(false)
    | Label (MergeLabel label | BranchLabel label) ->
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
    let value_ = function
      | Int i -> const_int i32 i
      | Fun_ref f -> get_active_version the_module program f
      | (Nil|Bool _|Array _) -> assert(false)
    in
    let simple = function
      | Var v             ->
          let id = var_id v in
          begin match id with
          | Arg, x     ->
                Hashtbl.find llvm_scope id
          | Instr i, x ->
              let alloca = (try Hashtbl.find llvm_scope id with
                            | Not_found -> raise (Error "unknown variable name")) in
              build_load alloca x builder
          end
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
        let id = (Instr pc, var) in
        let alloca = Hashtbl.find llvm_scope id in
        (* Store value into alloc *)
        ignore(build_store start_val alloca builder);
        ()
    | Call (l, var, f, args) ->
        let func_ref = dump_expr f in
        let func_args = List.map dump_expr args in
        let func_args = Array.of_list func_args in
        let ret_val = build_call func_ref func_args "calltmp" builder in

        let id = (Instr pc, var) in
        let alloca = Hashtbl.find llvm_scope id in
        (* Store value into alloc *)
        ignore(build_store ret_val alloca builder);
        ()
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
    | Label (MergeLabel label | BranchLabel label) ->
        let bb = Hashtbl.find labels label in
        position_at_end bb builder; ()
    | Goto label ->
        let bb = Hashtbl.find labels label in
        build_br bb builder; ()
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
  let the_module = create_module context "Sourir LLVM Jit" in
  let open Types in
  List.iter (fun ({name; formals; body} as sourir_function) ->
      List.iter (fun version ->
        let llvm_function = func_decl the_module (String.concat "::" [name; version.label]) formals in
        ()
      ) body
    )(program.main :: program.functions);

  List.iter (fun ({name; formals; body} as sourir_function) ->
      List.iter (fun version ->
        let llvm_function = func_lookup the_module (String.concat "::" [name; version.label]) in
        let scope = Scope.infer_decl (Analysis.as_analysis_input sourir_function version) in
        generate_instr program the_module llvm_function scope formals version.instrs;
        Llvm_analysis.assert_valid_function llvm_function) body
      ) (program.main :: program.functions);
  the_module
