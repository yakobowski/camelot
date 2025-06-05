open Canonical

let string_of_warnloc : Warn.warn_loc -> string =
  fun {file; line_start; line_end; col_start; col_end} ->
  "File " ^ file ^ ", " ^ 
  (if line_start = line_end then
     "line " ^ (string_of_int line_start)
   else
     "lines " ^ (string_of_int line_start) ^ "-" ^ (string_of_int line_end)
  ) ^ ", " ^
  (
    "columns: " ^ (string_of_int col_start) ^ "-" ^ (string_of_int col_end)
  )    

let json_of_hint : Hint.hint -> Yojson.Basic.t =
  fun {loc; raw; fix; violation} ->
  `Assoc [
    ("filename", `String loc.file);
    ("line", `List [`Int loc.line_start; `Int loc.line_end]);
    ("col", `List [`Int loc.col_start; `Int loc.col_end]);
    ("source", `String raw);
    ("fix", `String fix);
    ("violation", `String violation)
  ]

let common_loc_1 : Canonical.Warn.warn_loc =
  {Canonical.Warn.file = "test.ml"; line_start = 1; line_end = 1; col_start = 0; col_end = 10}

let common_loc_2 : Canonical.Warn.warn_loc =
  {Canonical.Warn.file = "test1.ml"; line_start = 1; line_end = 1; col_start = 0; col_end = 10}

let common_loc_3 : Canonical.Warn.warn_loc =
  {Canonical.Warn.file = "test2.ml"; line_start = 2; line_end = 2; col_start = 5; col_end = 15}

let common_hint_1 : Canonical.Hint.hint =
  {Canonical.Hint.loc = common_loc_1; raw = "raw code"; fix = "fixed code"; violation = "violation message"}

let common_hint_2 : Canonical.Hint.hint =
  {Canonical.Hint.loc = common_loc_2; raw = "raw1"; fix = "fix1"; violation = "violation1"}

let common_hint_3 : Canonical.Hint.hint =
  {Canonical.Hint.loc = common_loc_3; raw = "raw2"; fix = "fix2"; violation = "violation2"}

let common_hints_list : Canonical.Hint.hint list = [common_hint_2; common_hint_3]

(* Utility display methods *)
(* TODO: in the mli file, don't expose these methods  *)
let display_verbose : Hint.hint -> unit =
  fun {loc; raw; fix; violation} ->
  let sep = [ANSITerminal.cyan] in
  let pat = [ANSITerminal.magenta] in
  let warn = [ANSITerminal.yellow] in
  let sugg = [ANSITerminal.green; ANSITerminal.Bold] in
  let m_warn = string_of_warnloc loc in
  ANSITerminal.print_string sep
    "(* ------------------------------------------------------------------------ *)\n";
  print_endline @@ m_warn ;
  ANSITerminal.print_string warn "Warning:";
  ANSITerminal.print_string [] ("\n\t" ^ violation ^ "\n");
  ANSITerminal.print_string pat ("You wrote:");
  ANSITerminal.print_string [] ("\n\t " ^ raw ^ "\n");
  ANSITerminal.print_string sugg ("Consider:");
  ANSITerminal.print_string [] ("\n\t" ^ fix ^ "\n\n")

let display_brief : Hint.hint -> unit =
  fun hint ->
  let warn = [ANSITerminal.yellow] in
  let m_warn = string_of_warnloc hint.loc in
  print_endline @@ m_warn ;
  ANSITerminal.print_string warn "Warning:";
  ANSITerminal.print_string [] (hint.violation ^ "\n\n")


(* Display methods to expose *)
(* TODO: Write an mli file exposing as appropriate *)

let student_display : Hint.hint list -> unit = fun l ->
  if List.length l > 0 then List.iter (display_verbose) l else
    let set = [ANSITerminal.green; ANSITerminal.Bold] in
    ANSITerminal.print_string set "No style violations\n"

let ta_display : Hint.hint list -> unit = fun l ->
  List.iter (display_brief) l;
  let score = "Final score: " ^ string_of_int (Grade.simple l ) ^ " mistakes" in
  let set = [ANSITerminal.green; ANSITerminal.Bold] in
  ANSITerminal.print_string set score

let gradescope_display : Hint.hint list -> unit = fun l ->
  let score = string_of_int (Grade.simple l) in
  print_string score


let json_display : Hint.hint list -> unit = fun l ->
  print_string (Yojson.Basic.to_string (`List (List.map json_of_hint l)))

let capture_stdout_cleaned_for_test (thunk: unit -> unit) : unit =
  let module Unix = Unix in
  let module Str = Str in
  let module In_channel = Stdlib.In_channel in

  let old_stdout = Unix.dup Unix.stdout in
  let r_fd, w_fd = Unix.pipe () in

  Unix.dup2 w_fd Unix.stdout;
  Unix.close w_fd;

  thunk ();
  Stdlib.flush Stdlib.stdout;

  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;

  let ic = Unix.in_channel_of_descr r_fd in
  let captured_content = In_channel.input_all ic in
  In_channel.close ic;

  let cleaned_output = Str.global_replace (Str.regexp "\\[[0-9;]*m") "" captured_content in
  print_string cleaned_output
;;

let%expect_test "string_of_warnloc with different line_start and line_end" =
  let open Canonical.Warn in
  let loc = { file = "test.ml"; line_start = 1; line_end = 2; col_start = 0; col_end = 10 } in
  print_endline (string_of_warnloc loc);
  [%expect{| File test.ml, lines 1-2, columns: 0-10 |}]

let%expect_test "json_of_hint" =
  print_endline (Yojson.Basic.pretty_to_string (json_of_hint common_hint_1));
  [%expect{|
    {
      "filename": "test.ml",
      "line": [ 1, 1 ],
      "col": [ 0, 10 ],
      "source": "raw code",
      "fix": "fixed code",
      "violation": "violation message"
    } |}]

let%expect_test "display_brief" =
  capture_stdout_cleaned_for_test (fun () -> display_brief common_hint_1);
  [%expect{|
File test.ml, line 1, columns: 0-10
Warning:violation message

|}]

let%expect_test "student_display with empty list" =
  capture_stdout_cleaned_for_test (fun () -> student_display []);
  [%expect{|
No style violations
|}]

let%expect_test "ta_display" =
  capture_stdout_cleaned_for_test (fun () -> ta_display common_hints_list);
  [%expect{|
File test1.ml, line 1, columns: 0-10
Warning:violation1

File test2.ml, line 2, columns: 5-15
Warning:violation2

Final score: 2 mistakes|}]

let%expect_test "gradescope_display" =
  gradescope_display common_hints_list;
  [%expect{| 2 |}]

let%expect_test "json_display" =
  json_display common_hints_list;
  [%expect{| [{"filename":"test1.ml","line":[1,1],"col":[0,10],"source":"raw1","fix":"fix1","violation":"violation1"},{"filename":"test2.ml","line":[2,2],"col":[5,15],"source":"raw2","fix":"fix2","violation":"violation2"}] |}]
