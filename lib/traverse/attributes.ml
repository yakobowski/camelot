(* Detection of camelot-specific warnings, that can be used locally or globally
   to deactivate some rules *)

open Parsetree

(* Attribute name *)
let attr_name = "camelot.warning"
let is_camelot_attr attr = attr.attr_name.Location.txt = attr_name


(* Disabled rules are represented by the set of their names *)
module RulesSet = Set.Make(String)

exception Invalid_attribute of Location.t * string

let fail loc msg =
  Format.eprintf "%a: %s@." Location.print_loc loc msg;
  exit 1

(* Extract the payload of a camelot warning attribute, supposed to be a
   ',' delimited string. The actual parsing is done in later functions. *)
let extract_payload attr =
  let err = Invalid_attribute (attr.attr_loc, "camelot.warning payload must be a string") in
  match attr.attr_payload with
  | PStr [{pstr_desc = Pstr_eval (e, _); _}] ->
    begin match e.pexp_desc with
      | Pexp_constant (Pconst_string (s, _, _)) -> s
      | _ -> raise err
    end
  | _ -> raise err


(* Parse the payload of a camelot attribute, and return the re-enabled
   or disabled rules *)
let parse_payload loc payload_str =
  let rules = String.split_on_char ',' payload_str in
  let aux acc s =
    if s = "" then acc
    else
      let s' = String.sub s 1 (String.length s - 1) in
      match s.[0] with
      | '+' -> (true, s') :: acc
      | '-' -> (false, s') :: acc
      | _ -> raise (Invalid_attribute (loc, "unknown directive " ^ s))
  in
  List.fold_left aux [] rules


(* Add [to_disable] to some already deactivated rules *)
let disable_rule disabled to_disable =
  RulesSet.union disabled (RulesSet.singleton to_disable)

(* Remove [to_reenable] from a set of disabled rules.
   Checks that this makes sens *)
let reenable_rule loc disabled to_reenable =
if RulesSet.mem to_reenable disabled then
  RulesSet.remove to_reenable disabled 
else
  let msg = "cannot reenable rule that was not disabled: " ^ to_reenable in
  fail loc msg

(* Take [rules] into account on top of [disabled], respecting
   the initial ordering. *)
let update_rules loc disabled rules =
    (* Disable the rules in the current scope *)
    let do_one (enabled, rule) acc =
        if enabled then reenable_rule loc acc rule
        else disable_rule acc rule
    in
    (* Respect the ordering *)
    List.fold_right do_one rules disabled


(* Handling of global attributes such as [@@@camelot.warning "..."] *)

type disabled_rules_scoped =
  { mutable all_scopes: RulesSet.t; (* Rules disabled in the current declaration scope, or in the previous ones *)
    prev_scope: disabled_rules_scoped option; (* Information in the previous scope *)
  }

let initial_scope = {
  all_scopes = RulesSet.empty;
  prev_scope = None
}

let current_rules = ref initial_scope


(** Exported identifiers *)

(* Function called by the iterator when entering a new declarative scope *)
let push_scope () =
  current_rules := {
    all_scopes = !current_rules.all_scopes;
    prev_scope = Some !current_rules}

(* Function called by the iterator when leaving the current declarative scope *)    
let pop_scope () =
  match !current_rules.prev_scope with
  | Some prev ->
    current_rules := prev
  | None ->
    raise (Invalid_argument "cannot pop scope when no scope is active")

(* Function called by the iterator when encountering a global attribute *)    
let push_attribute attr =
  if is_camelot_attr attr then
    let loc = attr.attr_loc in
    let payload = extract_payload attr in
    (* Rules to enable and disable *)
    let rules = parse_payload loc payload in
    (* Make sure a scope has been pushed *)
    assert (!current_rules <> initial_scope);
    (* Apply the payload to the current scope *)
    !current_rules.all_scopes <-
      update_rules loc !current_rules.all_scopes rules


let filter_out_checks attrs checks =
  let loc, local_disabled =
    try
      (* Find potential locally disabled rules *)
      let attr = List.find is_camelot_attr attrs in
      let payload_str = extract_payload attr in
      attr.attr_loc,
      parse_payload attr.attr_loc payload_str
    with
    | Invalid_attribute (loc, msg) -> fail loc msg
    | Not_found ->
      (* Default in case there is no local attribute *)
      Location.none, []
  in
    (* Add locally disabled ones to global ones, giving precedence
       to the local ones. *)
    let with_local =
      update_rules loc !current_rules.all_scopes local_disabled
    in
    (* Filter out checks accordingly *)
    let check_ok (name, _) = not (RulesSet.mem name with_local) in
    List.filter check_ok checks
