(* Detection of camelot-specific warnings, that can be used locally or globally
   to deactivate some rules *)

exception Invalid_attribute of Location.t * string

module RulesSet : Set.S with type elt = String.t

type disabled_rules_scoped = {
  mutable all_scopes: RulesSet.t;
  prev_scope: disabled_rules_scoped option;
}

val initial_scope : disabled_rules_scoped
val current_rules : disabled_rules_scoped ref

val extract_payload : Parsetree.attribute -> string
val parse_payload : Location.t -> string -> (bool * string) list

val disable_rule : RulesSet.t -> string -> RulesSet.t
val reenable_rule : Location.t -> RulesSet.t -> string -> RulesSet.t
val update_rules : Location.t -> RulesSet.t -> (bool * string) list -> RulesSet.t

val push_scope: unit -> unit
val pop_scope: unit -> unit
val push_attribute: Parsetree.attribute -> unit

val filter_out_checks: Parsetree.attributes -> (string * 'a) list -> (string * 'a) list