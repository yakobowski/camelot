open Canonical

(** The Check interface - fill this out with the kind of check you would like to make. *)
module type CHECK = sig

  (** Type of the ast element being checked *)
  type t

  (** The violation associated with this check *)
  val violation : Hint.violation

  (** A method that performs a check, given a lint context, and adds the hint to the list*)
  val check : Hint.hint list ref -> ?rules:Arthur.rule list -> t Pctxt.pctxt -> unit

  (** Name of the check. *)
  val name : string

end

type 'a callback = {
  pre_callback: 'a -> unit;
  (** Callback called before the analysis of the node *)

  post_callback: 'a -> unit;
  (** Callback called after the analysis of the node and its children *)
}

(* Full signature: a check with callbacks to execute before and after the
  ast element is visited. *)
module type CHECK_WITH_CALLBACK = sig

  include CHECK
  val callback: t callback
end

module Add_Callbacks
  (C : CHECK) :
CHECK_WITH_CALLBACK with type t = C.t =
struct
  include C

  let callback = {
    pre_callback = (fun _ -> ());
    post_callback = (fun _ -> ());
  }
end

(** Signature for expression checking rules *)
module type EXPRCHECK = CHECK with type t = Parsetree.expression_desc
module type EXPRCHECKCALLBACK = CHECK_WITH_CALLBACK with type t = Parsetree.expression_desc
let expr_check_with_callbacks (c: (module EXPRCHECK)) : (module EXPRCHECKCALLBACK) =
  let module C = (val c : EXPRCHECK) in
  let module CC = Add_Callbacks(C) in
  (module CC : EXPRCHECKCALLBACK)

(** Signature for file (top-level) checking rules *)

(** Signature for structure (top-level) checking rules *)
module type STRUCTURECHECK = CHECK with type t = Parsetree.structure_item_desc

(** Signature for lexical checking rules *)
module type LEXICALCHECK = CHECK with type t = Pctxt.file
