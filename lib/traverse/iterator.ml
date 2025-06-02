open Ast_iterator
open Descent
open Find


(** Iterates through the ast structure, applying the given iterator *)
let apply_iterator (structure: Parsetree.structure) (iter: Ast_iterator.iterator) : unit = 
  iter.structure iter structure


(* Iterator for traversing expressions *)
let expr_iterator (mk_handlers: _ -> _ Find.check_handlers) (iterator: Ast_iterator.iterator) (expr: Parsetree.expression) : unit =
  let handlers = mk_handlers expr in
  (* Call the pre callbacks *)
  handlers.pre expr;
  (* Do the expression itself *)
  handlers.on expr;
  (* Do its children *)
  E.iter iterator expr;
  (* Call the post callbacks *)
  handlers.Find.post expr

(* Iterator for traversing structure_items (top level modules declarations) *)
let structure_item_iterator (handler: Parsetree.structure_item -> 'a) (iterator: Ast_iterator.iterator) (structure_item: Parsetree.structure_item) : unit =
  handler structure_item;
  M.iter_structure_item iterator structure_item

let structure_iterator (handler: Parsetree.structure -> 'a) (iterator: Ast_iterator.iterator)
    (payload: Parsetree.structure) : unit =
  handler payload;
  ST.iter iterator payload


(** Given a list ref and a filename, produce an iterator that runs the pass_checks method at each expression node in the
    OCaml ast. This will mutate the store by appending new hints the checkers find as they analyse the source code.
*)
let make_linterator store fname = {
  Ast_iterator.default_iterator with
    expr = expr_iterator (pass_exprs store fname);
    structure_item = structure_item_iterator (pass_structures store fname);
    structure = structure_iterator (pass_file store fname);
}
