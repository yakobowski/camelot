(**
   HOF's passed to the iterator that run the linting rules.
*)
open Canonical
open Parsetree

(* Operations to perform on a CFG node, taking callbacks into account. *)
type 'a check_handlers = {
  pre:  'a -> unit;
  on :  'a -> unit;
  post: 'a -> unit;
}


(* force computation *)
let cfg = Arthur.parse ()


let currently_linting : string ref = ref ""


(** Secret dependency on arthur here - we pull up the config at this point, since
    the linter needs to know what the arthur.json looks like! 
*)


(* Compute the checks to run on a given expression node *)
let pass_exprs (store: Hint.hint list ref) (f: string) (expr : expression) : expression check_handlers =
  let checks =
    Style.Checkers.expr_checks
    (* Extract the useful functions from the module *)
    |> List.map (fun c ->
      let module C = (val c : Style.Check.EXPRCHECKCALLBACK) in
      C.name, (C.check, C.callback))
    (* Fetch the lint config *)
    |> Arthur.extract ( Lazy.force cfg )
    |> Arthur.refine (Lazy.force cfg) !currently_linting
    (* Take local and global attributes into account *)
    |> Attributes.filter_out_checks expr.pexp_attributes
  in
  (* Build all 3 closures *)
  let pre expr =  List.iter (fun (_, (_, cb)) -> cb.Style.Check.pre_callback expr.pexp_desc) checks in
  let post expr = List.iter (fun (_, (_, cb)) -> cb.Style.Check.post_callback expr.pexp_desc) checks in
  let on expr =
    let pc = Pctxt.ctxt_of_expr f expr in
    let rules = Arthur.rules (Lazy.force cfg) in
    List.iter (fun (_, (check, _)) -> check store ~rules pc) checks
  in
  {pre; on; post}


let set_toplevel : Parsetree.structure_item -> unit = fun i ->
  begin match i.pstr_desc with
    | Pstr_value (_, [vb]) ->
      begin match vb.pvb_pat.ppat_desc with
        | Ppat_var {txt = i; loc = _} -> currently_linting := i
        | _ -> ()
      end
    | _ -> ()
  end



let pass_structures (store: Hint.hint list ref) (f: string) (structure : Parsetree.structure_item) : unit =
  (* Flag the currently linted toplevel function *)
  set_toplevel structure;
  let pc = Pctxt.ctxt_of_structure f structure in
  let checks =
    Style.Checkers.struct_checks
    |> List.map (fun c -> let module C = (val c : Style.Check.STRUCTURECHECK) in C.name, C.check)
    |> Arthur.extract (Lazy.force cfg)
    |> Arthur.refine (Lazy.force cfg) !currently_linting
    (* there is no ideal attribute here. use only the global ones *)
    |> Attributes.filter_out_checks []
  in
  let rules = Arthur.rules (Lazy.force cfg) in
  List.iter (fun (_, check) -> check store ~rules pc) checks


let pass_file (store: Hint.hint list ref) (f: string) (_payload: Parsetree.structure) : unit =
  let ch = open_in f in
  let pc = Pctxt.ctxt_for_lexical f ch in
  let checks =
    Style.Checkers.lexical_checks
    |> List.map (fun c -> let module C = (val c : Style.Check.LEXICALCHECK) in C.name, C.check)
    |> Arthur.extract (Lazy.force cfg)
  in
  let rules = Arthur.rules (Lazy.force cfg) in
  List.iter (fun (_, check) -> check store ~rules pc) checks;
  close_in ch
