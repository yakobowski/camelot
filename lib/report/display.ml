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

let%expect_test "string_of_warnloc with different line_start and line_end" =
  let open Canonical.Warn in
  let loc = { file = "test.ml"; line_start = 1; line_end = 2; col_start = 0; col_end = 10 } in
  print_endline (string_of_warnloc loc);
  [%expect{| File test.ml, lines 1-2, columns: 0-10 |}]

let%expect_test "json_of_hint" =
  let hint : Canonical.Hint.hint = (* Explicitly type hint *)
    {
      loc = { Canonical.Warn.file = "test.ml"; line_start = 1; line_end = 1; col_start = 0; col_end = 10 };
      raw = "raw code";
      fix = "fixed code";
      violation = "violation message";
    }
  in
  print_endline (Yojson.Basic.pretty_to_string (json_of_hint hint));
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
  let hint : Canonical.Hint.hint = (* Explicitly type hint *)
    {
      loc = { Canonical.Warn.file = "test.ml"; line_start = 1; line_end = 1; col_start = 0; col_end = 10 };
      raw = "raw code";
      fix = "fixed code";
      violation = "violation message";
    }
  in
  (* Redirect stdout to capture printed output *)
  let old_stdout = Unix.dup Unix.stdout in
  let r, w = Unix.pipe () in
  Unix.dup2 w Unix.stdout;
  Unix.close w;
  display_brief hint;
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  let output_channel = Unix.in_channel_of_descr r in
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line output_channel)
     done
   with End_of_file -> ());
  close_in output_channel;
  (* Remove ANSI codes for consistent testing *)
  let cleaned_output = Str.global_replace (Str.regexp "\\[[0-9;]*m") "" (Buffer.contents buffer) in
  print_endline cleaned_output;
  [%expect{|
    Warning:violation message

    File test.ml, line 1, columns: 0-10 |}]

let%expect_test "student_display with empty list" =
  (* Redirect stdout to capture printed output *)
  let old_stdout = Unix.dup Unix.stdout in
  let r, w = Unix.pipe () in
  Unix.dup2 w Unix.stdout;
  Unix.close w;
  student_display [];
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  let output_channel = Unix.in_channel_of_descr r in
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line output_channel)
     done
   with End_of_file -> ());
  close_in output_channel;
  (* Remove ANSI codes for consistent testing *)
  let cleaned_output = Str.global_replace (Str.regexp "\\[[0-9;]*m") "" (Buffer.contents buffer) in
  print_endline cleaned_output;
  [%expect{| No style violations |}]

let%expect_test "ta_display" =
  let hints : Canonical.Hint.hint list = (* Explicitly type hints *)
    [
      {
        loc = { Canonical.Warn.file = "test1.ml"; line_start = 1; line_end = 1; col_start = 0; col_end = 10 };
        raw = "raw1";
        fix = "fix1";
        violation = "violation1";
      };
      {
        loc = { Canonical.Warn.file = "test2.ml"; line_start = 2; line_end = 2; col_start = 5; col_end = 15 };
        raw = "raw2";
        fix = "fix2";
        violation = "violation2";
      };
    ]
  in
  (* Redirect stdout to capture printed output *)
  let old_stdout = Unix.dup Unix.stdout in
  let r, w = Unix.pipe () in
  Unix.dup2 w Unix.stdout;
  Unix.close w;
  ta_display hints;
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  let output_channel = Unix.in_channel_of_descr r in
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line output_channel);
       Buffer.add_char buffer '\n'; (* Add newline as input_line removes it *)
     done
   with End_of_file -> ());
  close_in output_channel;
  (* Remove ANSI codes for consistent testing *)
  let cleaned_output = Str.global_replace (Str.regexp "\\[[0-9;]*m") "" (Buffer.contents buffer) in
  print_string cleaned_output; (* Use print_string to preserve newlines *)
  [%expect{|
    Warning:violation2

    Final score: 2 mistakesFile test1.ml, line 1, columns: 0-10
    Warning:violation1

    File test2.ml, line 2, columns: 5-15 |}]

let%expect_test "gradescope_display" =
  let hints : Canonical.Hint.hint list = (* Explicitly type hints *)
    [
      {
        loc = { Canonical.Warn.file = "test1.ml"; line_start = 1; line_end = 1; col_start = 0; col_end = 10 };
        raw = "raw1";
        fix = "fix1";
        violation = "violation1";
      };
      {
        loc = { Canonical.Warn.file = "test2.ml"; line_start = 2; line_end = 2; col_start = 5; col_end = 15 };
        raw = "raw2";
        fix = "fix2";
        violation = "violation2";
      };
    ]
  in
    (* Redirect stdout to capture printed output *)
  let old_stdout = Unix.dup Unix.stdout in
  let r, w = Unix.pipe () in
  Unix.dup2 w Unix.stdout;
  Unix.close w;
  gradescope_display hints;
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  let output_channel = Unix.in_channel_of_descr r in
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line output_channel)
     done
   with End_of_file -> ());
  close_in output_channel;
  print_endline (Buffer.contents buffer);
  [%expect{| 2 |}]

let%expect_test "json_display" =
  let hints : Canonical.Hint.hint list = (* Explicitly type hints *)
    [
      {
        loc = { Canonical.Warn.file = "test1.ml"; line_start = 1; line_end = 1; col_start = 0; col_end = 10 };
        raw = "raw1";
        fix = "fix1";
        violation = "violation1";
      };
      {
        loc = { Canonical.Warn.file = "test2.ml"; line_start = 2; line_end = 2; col_start = 5; col_end = 15 };
        raw = "raw2";
        fix = "fix2";
        violation = "violation2";
      };
    ]
  in
  (* Redirect stdout to capture printed output *)
  let old_stdout = Unix.dup Unix.stdout in
  let r, w = Unix.pipe () in
  Unix.dup2 w Unix.stdout;
  Unix.close w;
  json_display hints;
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  let output_channel = Unix.in_channel_of_descr r in
  let buffer = Buffer.create 256 in
  (try
     let s = In_channel.input_all output_channel in (* Read all content *)
     Buffer.add_string buffer s
   with End_of_file -> ());
  close_in output_channel;
  let captured_content = Buffer.contents buffer in
  print_endline captured_content; (* Directly print the compact JSON *)
  [%expect{| [{"filename":"test1.ml","line":[1,1],"col":[0,10],"source":"raw1","fix":"fix1","violation":"violation1"},{"filename":"test2.ml","line":[2,2],"col":[5,15],"source":"raw2","fix":"fix2","violation":"violation2"}] |}]
