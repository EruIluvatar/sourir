open Instr

let () =
  match Sys.argv.(1) with
  | exception _ ->
    Printf.eprintf
      "You should provide a Sourir file to parse as command-line argument.\n\
       Example: %s examples/sum.sou\n%!"
      Sys.executable_name;
    exit 1
  | path ->
    let program =
      try Parse.parse_file path
      with Parse.Error error ->
        Parse.report_error error;
        exit 2
    in
    let quiet = Array.mem "--quiet" Sys.argv in
    let prune = Array.mem "--prune" Sys.argv in
    let codemotion = Array.mem "--cm" Sys.argv in
    let constprop = Array.mem "--prop" Sys.argv in
    let lifetime = Array.mem "--lifetime" Sys.argv in

    begin try Scope.check_program program with
    | Scope.ScopeExceptionAt (f, v, e) ->
      begin
        Printf.eprintf "Error in function %s version %s " f v;
        begin match e with
        | Scope.UndeclaredVariable (xs, pc) ->
          let l = pc+1 in
          begin match VarSet.elements xs with
            | [x] -> Printf.eprintf
                       "line %d: Variable %s is not declared.\n%!"
                       l x
            | xs -> Printf.eprintf
                      "line %d: Variables {%s} are not declared.\n%!"
                      l (String.concat ", " xs)
          end;
        | Scope.UninitializedVariable (xs, pc) ->
          let l = pc+1 in
          begin match VarSet.elements xs with
            | [x] -> Printf.eprintf
                       "line %d: Variable %s might be uninitialized.\n%!"
                       l x
            | xs -> Printf.eprintf
                      "line %d: Variables {%s} might be uninitialized.\n%!"
                      l (String.concat ", " xs)
          end;
        | Scope.ExtraneousVariable (xs, pc) ->
          let l = pc+1 in
          let func = lookup_fun program f in
          let version = lookup_version func v in
          let annot = match version.annotations with | Some a -> a | None -> assert(false) in
          let annot_vars = match annot.(pc) with
            | None | Some (AtLeastScope _) ->
              (* we know from the exception-raising code that this cannot happen *)
              assert (false)
            | Some (ExactScope vars) ->
              VarSet.elements vars |>
              String.concat ", " |> Printf.sprintf " {%s}" in
          begin match VarSet.elements xs with
            | [x] -> Printf.eprintf
                       "line %d: Variable %s is present in scope but missing \
                       from the scope annotation%s.\n%!"
                       l x annot_vars
            | xs -> Printf.eprintf
                      "line %d: Variables {%s} are present in scope \
                       but missing from the scope annotation%s.\n%!"
                      l (String.concat ", " xs) annot_vars
          end;
        | Scope.DuplicateVariable (xs, pc) ->
          let l = pc+1 in
          begin match VarSet.elements xs with
            | [x] -> Printf.eprintf
                       "line %d: Variable %s is declared more than once.\n%!"
                       l x
            | xs -> Printf.eprintf
                      "line %d: Variables {%s} are declared more than once.\n%!"
                      l (String.concat ", " xs)
          end;
        | Scope.IncompatibleScope (scope1, scope2, pc) ->
          let func = lookup_fun program f in
          let version = lookup_version func v in
          let instrs = version.instrs in
          Disasm.pretty_print_version stderr (v, instrs);
          Scope.explain_incompatible_scope stderr scope1 scope2 pc;
          flush stderr;
        | _ -> assert(false)
        end;
        exit 1
      end
    end;

    begin try Check.well_formed program with
    | Check.MissingMain ->
      Printf.eprintf "Program is missing an explicit or implicit main function\n";
      exit 1
    | Check.DuplicateFunctionDeclaration f ->
      Printf.eprintf "Duplicate function declaration %s\n" f;
      exit 1
    | Check.InvalidFunctionDeclaration f ->
      Printf.eprintf "Function %s is invalid\n" f;
      exit 1
    | Check.DuplicateVersion (f, v) ->
      Printf.eprintf "Version %s in function %s is define twice\n" v f;
      exit 1
    | Check.EmptyFunction f ->
      Printf.eprintf "Function %s has no body\n" f;
      exit 1
    | Check.DuplicateParameter (f, x) ->
      Printf.eprintf "Function %s : parameter %s is given twice\n" f x;
      exit 1
    | Check.ErrorAt (f, v, e) ->
      Printf.eprintf "Error in function %s version %s: " f v;
      begin match[@warning "-4"] e with
      | Check.FunctionDoesNotExist f' ->
        Printf.eprintf "called function %s does not exist\n" f';
      | Check.VersionDoesNotExist (f', v') ->
        Printf.eprintf "osr target %s %s does not exist\n" f' v';
      | Check.InvalidNumArgs pc ->
        Printf.eprintf "at line %d: invalid number of arguments\n" (pc+1);
      | Check.InvalidArgument (pc, expression) ->
        Printf.eprintf "at line %d: invalid argument\n" (pc+1);
      | _ -> assert(false)
      end;
      exit 1
    end;

    let program = if prune
      then
        let opt = { program with main = Transform.branch_prune program.main } in
        if not quiet then Printf.printf "\n** After speculative branch pruning:\n%s" (Disasm.disassemble_s opt);
        opt
      else program
    in

    let program = if codemotion
      then
        let opt = { program with main = Transform.hoist_assignment program.main } in
        if not quiet then Printf.printf "\n** After trying to hoist one assignment:\n%s" (Disasm.disassemble_s opt);
        opt
      else program
    in

    let program = if constprop
      then
        let opt = { program with main = Constantfold.const_prop program.main } in
        if not quiet then Printf.printf "\n** After constant propagation:\n%s" (Disasm.disassemble_s opt);
        opt
      else program
    in

    let program = if lifetime
      then
        let opt = { program with main = Transform.minimize_lifetimes program.main } in
        if not quiet then Printf.printf "\n** After minimizing lifetimes:\n%s" (Disasm.disassemble_s opt);
        opt
      else program
    in

    Scope.check_program program;

    let conf = Eval.run_interactive IO.stdin_input program in
    let open Eval in
    match conf.status with
    | Running -> assert(false)
    | Stopped None
      -> exit 0
    | Stopped (Some (Lit (Int n))) ->
      exit n
    | Stopped (Some (Lit (Bool b))) ->
      exit (if b then 1 else 0)
    | Stopped (Some (Lit Nil)) ->
      exit 0

