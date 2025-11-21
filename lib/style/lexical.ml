open Canonical
open Check

(** --------- Checks rules: lines that exceed 80 characters in a given file ------------ *)
module LineLength : LEXICALCHECK = struct

  type t = Pctxt.file

  let violation = "exceeding the 80 character line limit. Only showing (1) such violation of this kind, although there may be others - fix this and re-run the linter to find them."

  let check st ~rules (Pctxt.L {source; pattern = Pctxt.F chan}) =
    let max_len =
      let rule = List.find_opt (fun (Arthur.Rule (name, _)) -> name = "LineLength") rules in
      match rule with
      | Some (Arthur.Rule (_, `Int i)) -> i
      | Some (Arthur.Rule (_, j)) ->
        let loc = Canonical.Warn.warn_loc source 1 1 0 0 in
        let msg = Printf.sprintf "invalid value for LineLength: %s" (Yojson.Basic.to_string j) in
        st := Hint.mk_hint loc source msg "invalid configuration" :: !st;
        80
      | _ -> 80
    in
    let filestream : (int * string) Stream.t =
      (* Stream.from 0 indexes file lines, but line numbers start at 1. Have to increment so that the line numbers are consistent with editors :) *)
      Stream.from
        (fun line -> try (Some (line + 1, input_line chan))
          with End_of_file -> None
        ) in

    Stream.iter (fun (line_no, line) ->
        if String.length line > max_len then
          st := Hint.line_hint source line_no line :: !st
      ) filestream

  let name = "LineLength"

end
