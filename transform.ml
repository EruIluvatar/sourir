open Instr

let remove_empty_jmp prog =
  let pred = Analysis.predecessors prog in
  let succ = Analysis.successors prog in
  let rec remove_empty_jmp pc =
    let pc' = pc + 1 in
    if pc' = Array.length prog then [prog.(pc)] else
      match[@warning "-4"] (prog.(pc), prog.(pc')) with
      | (Goto l1, Label l2) when l1 = l2 && List.length pred.(pc') = 1 ->
          remove_empty_jmp (pc+2)
      | (Label _, _) when pred.(pc) = [pc-1] && succ (pc-1) = [pc] ->
          (* A label is unused if the previous instruction is the only predecessor
           * unless the previous instruction jumps to it. The later can happen
           * if its a goto (then we already remove it -- see above) or if its a branch (which
           * is excluded by the second tests "succ (pc-1) = [pc]")
           * TODO: we should implement some generic api for instructions like, "Instr.is_jmp" *)
          remove_empty_jmp pc'
      | (_, _) ->
          prog.(pc) :: remove_empty_jmp pc'
  in
  remove_empty_jmp 0

let remove_dead_code prog entry=
  let dead_code =
    let merge _ _ = None in
    let update _ _ = () in
    Analysis.forward_analysis_from entry () prog merge update
  in
  let rec remove_dead_code pc =
    let pc' = pc+1 in
    if pc = Array.length prog then [] else
      match[@warning "-4"] dead_code.(pc), prog.(pc) with
      (* Comments are considered live, if the next instruction is live.
       * This prevents removing comments above jump labels *)
      | None, Comment c ->
          if pc' = Array.length prog then [] else
            begin match dead_code.(pc') with
            | None -> remove_dead_code pc'
            | Some _ -> Comment c :: remove_dead_code pc'
            end
      | None, _ -> remove_dead_code pc'
      | Some _, _ -> prog.(pc) :: remove_dead_code pc'
  in
  remove_dead_code 0

module LabelSet = Set.Make(String)

let collect_labels prog =
  let rec labels pc =
    if pc = Array.length prog then LabelSet.empty else
      let pc' = pc + 1 in
      match[@warning "-4"] prog.(pc) with
      | Label l -> LabelSet.union (LabelSet.singleton l) (labels pc')
      | _ -> labels pc' in
  labels 0

let next_fresh_label used hint =
  let is_fresh l = match LabelSet.find l used with
    | exception Not_found -> true
    | x -> false
  in
  if is_fresh hint then hint else
    let l i = hint ^ "." ^ (string_of_int i) in
    let rec next_fresh i =
      let cur = l i in
      if is_fresh cur then cur else next_fresh (i+1) in
    next_fresh 0

module LabelMap = Map.Make(String)

(* Takes a list of globally occurring labels and a program
 * returns a copy of the program with all labels fresh and
 * an updated list of all occurring labels *)
let copy_fresh global_labels prog =
  let prog_labels = collect_labels prog in
  let rec freshened_labels labels todo =
    match todo with
    | [] -> LabelMap.empty
    | l :: tl ->
      let fresh = next_fresh_label labels l in
      let labels = LabelSet.add fresh labels in
      let rest = freshened_labels labels tl in
      LabelMap.add l fresh rest
  in
  let all_labels = LabelSet.union global_labels prog_labels in
  let prog_labels_map = freshened_labels all_labels (LabelSet.elements prog_labels) in
  let map l = LabelMap.find l prog_labels_map in
  let rec copy pc =
    if pc = Array.length prog then [] else
      let pc' = pc + 1 in
      match prog.(pc) with
      | Label l -> Label (map l) :: copy pc'
      | Goto l -> Goto (map l) :: copy pc'
      | Branch (exp, l1, l2) -> Branch (exp, map l1, map l2) :: copy pc'
      | Invalidate (exp, l, sc) -> Invalidate (exp, map l, sc) :: copy pc'
      | (Decl_const _ | Decl_mut _ | Assign _
        | Read _ | Print _ | Stop | Comment _) as i -> i :: copy pc'
  in
  let new_labels = LabelSet.map map prog_labels in
  let new_all_labels = LabelSet.union all_labels new_labels in
  (new_all_labels, Array.of_list (copy 0))

let branch_prune (prog, scope) =
  let rec branch_prune pc used_labels pruned landing_pads =
    if pc = Array.length prog then (pruned, landing_pads) else
    match scope.(pc) with
    | Dead -> assert(false)
    | Scope scope ->
      let pc' = pc + 1 in
      begin match[@warning "-4"] prog.(pc) with
      | Branch (exp, l1, l2) ->
        (* 1. Copy the program with fresh labels for the landing pad *)
        let used_labels, landing_pad = copy_fresh used_labels prog in
        (* 2. Insert a deopt target label into the landing pad before the
         *    original branch target. (Keep the original branch target
         *    label since other instructions might jump there too) *)
        let entry = resolve prog l2 in
        let deopt_label = next_fresh_label used_labels ("deopt_" ^ l2) in
        let used_labels = LabelSet.add deopt_label used_labels in
        let landing_pad = Array.concat [
          Array.sub landing_pad 0 entry;
          [| Label deopt_label |];
          Array.sub landing_pad entry ((Array.length landing_pad) - entry);
          (* In case the landing pad does not end in a stop this is needed
           * since we might fall through otherwise *)
          [| Stop |]
        ] in
        (* 3. Trim the landing pad to contain only the continuation
         *    part reachable from the entry label *)
        let landing_pad = Array.of_list (
            Comment ("Landing pad for " ^ deopt_label) ::
            remove_dead_code landing_pad entry) in
        (* 4. Replace the branch instruction by an invalidate *)
        let in_scope = Instr.VarSet.elements scope in
        let pruned = Invalidate (exp, deopt_label, in_scope) :: pruned in
        let pruned = Goto l1 :: pruned in
        let landing_pads = landing_pad :: landing_pads in
        branch_prune pc' used_labels pruned landing_pads
      | i -> branch_prune pc' used_labels (i :: pruned) landing_pads
      end
  in
  let rev_pruned, landing_pads = branch_prune 0 LabelSet.empty [] [] in
  (* In case the program does not end in a stop this is needed to not fall
   * through into the landing pads *)
  let rev_pruned = Stop :: rev_pruned in
  let final = Array.of_list (List.rev rev_pruned) in
  let combined = Array.concat (final :: landing_pads) in
  let cleanup = Array.of_list (remove_dead_code combined 0) in
  Array.of_list (remove_empty_jmp cleanup)
