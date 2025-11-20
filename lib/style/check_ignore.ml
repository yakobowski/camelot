open Check
open Canonical

(* Expr checkers that can decide to ignore some nodes during the visit. *)

module type EXPRCHECKIGNORE = sig
  include EXPRCHECK

  (* List of children of the argument that should be ignored during the
     recursive descent for the given rule. *)
  val children_to_ignore: Parsetree.expression_desc -> Parsetree.expression_desc list
end

module Make_Expr(X: EXPRCHECKIGNORE) : EXPRCHECKCALLBACK = struct
  include X
  type t = Parsetree.expression_desc

  (* List of nodes that should be ignored when checking for this rule. *)
  let ignore_nodes = ref []

  (* Potentially mark some sub-nodes as being ignored *)
  let pre_callback e =
    let to_skip = X.children_to_ignore e in
    (* Push on the stack if some nodes must be ignored.
       Push ourselves as the first element of the pair to
       make the post callback easier to write. *)
    if to_skip <> [] then
      ignore_nodes := (e, to_skip) :: !ignore_nodes

  let post_callback e =
    match !ignore_nodes with
    | [] -> ()
    | (e', _) :: rest ->
      (* Pop the top of the stack if it has been pushed by us. *)
      if e == e' then
        ignore_nodes := rest

  let callback = { pre_callback; post_callback }        

  (* Should [e] be ignored while testing this rule? *)
  let should_be_ignored e =
    List.exists (fun (_, l) -> List.exists ((==) e) l) !ignore_nodes

  (* Perform the check if the current node has not been marked
     as ignored. *)
  let check st ?rules (E {location; source; pattern} : Parsetree.expression_desc Pctxt.pctxt) =
    if not (should_be_ignored pattern) then
      X.check st ?rules (E {location; source; pattern})
end

let ignore_expr_check_with_callbacks (c: (module EXPRCHECKIGNORE)) : (module EXPRCHECKCALLBACK) =
  let module C = (val c : EXPRCHECKIGNORE) in
  let module CC = Make_Expr(C) in
  (module CC : EXPRCHECKCALLBACK)