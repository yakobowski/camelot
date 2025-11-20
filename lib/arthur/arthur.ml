type t = Arthur_parse.arthur
type rule = Arthur_parse.rule = Rule of string * Yojson.Basic.t


let lint_config_file : string ref = ref "arthur.json"



let print_config : t -> unit = fun v ->
  print_string @@ Arthur_parse.pp_arthur v

let parse : unit -> t lazy_t = fun _ ->
  lazy (Arthur_parse.json_to_arthur (Arthur_parse.from_file !lint_config_file))

let extract : t -> (string * 'a) list -> (string * 'a) list = fun c rules ->
  match c with
  | Arthur (_, Global (Disable toDisable), _, _) ->
    List.filter
      (fun (name,_) ->  not (List.exists (fun dis -> dis = name) toDisable)  )
      rules

let files : t -> string list = fun (Arthur(l, _, _, _)) -> l

let rules : t -> Arthur_parse.rule list = fun (Arthur (_,_,_,rs)) -> rs

let refine : t -> string -> (string * 'a) list -> (string * 'a) list = fun config func rules ->
  let open Arthur_parse in
  let config_has_rule (fs: func list) = List.exists (fun (Func (l,_)) -> l = func) fs in
  let rule_for_func f (fs: func list) =
    let return = List.find_map
        (fun (Func (l, Disable ls)) -> if l = f then Some ls else None ) fs in
    match return with
    | None -> failwith "literally impossible"
    | Some l -> l
  in 
  match config with
  | Arthur (_, _, fs, _) ->
    if config_has_rule fs then
      let toDisable = rule_for_func func fs in
      List.filter
        (fun (name, _) -> not (List.exists (fun dis -> dis = name) toDisable ) )
        rules
    else
      (* If the config has no rule - only globals apply *)
      rules


let default = Arthur_parse.default
