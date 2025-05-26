(* Detection of camelot-specific warnings, that can be used locally or globally
   to deactivate some rules *)

(** Function called by the iterator when entering a new declarative scope *)
val push_scope: unit -> unit

(** Function called by the iterator when leaving the current declarative scope *)    
val pop_scope: unit -> unit

(** Function called by the iterator when encountering a global attribute *)    
val push_attribute: Parsetree.attribute -> unit

(** Refine a list of checks according the local attributes passed as argument,
    and to the global rules. (Maintained imperatively in this file) *)
val filter_out_checks: Parsetree.attributes -> (string * 'a) list -> (string * 'a) list