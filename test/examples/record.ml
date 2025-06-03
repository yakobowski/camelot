type my_record_fields = {
  f1: int;
  f2: string;
  f3: int;
  one_very_long_field_name: int;
  another_very_long_field_name: int
};;

(* Cases that SHOULD trigger the warning *)

let func_triggers_1
  ({ f1; f2; _ } : my_record_fields) =
  f1 + String.length f2
;;

let func_triggers_2 x
  ({ f1; f2; f3; _ } : my_record_fields) y =
  x + f1 + (String.length f2) + f3 + y
;;

let func_triggers_3
  ({ f1; f2; _ } : my_record_fields) =
  string_of_int f1 ^ f2
;;

let func_triggers_4 x y
  ({ f1; f2; f3; _ } : my_record_fields) =
  x + y + f1 + (String.length f2) + f3
;;

let func_triggers_5
  ({ one_very_long_field_name;
     another_very_long_field_name; _ } : my_record_fields) =
  one_very_long_field_name + another_very_long_field_name
;;

let func_triggers_6
  ({ f1; f2; _ } : my_record_fields) =
  string_of_int f1 ^ f2
;;

let func_triggers_7
  (({ f1; f2; _ } as r) : my_record_fields) =
  r.f1 + String.length r.f2
;;

(* Cases that SHOULD NOT trigger the warning *)

let func_no_trigger_1 ({ f1; f2; _ } : my_record_fields) =
  f1 + String.length f2
;;

let func_no_trigger_2
  ({ f1; _ } : my_record_fields) =
  f1
;;

let func_no_trigger_3
  ({ f1 = _; f2 = _; _ } : my_record_fields) =
  0
;;

let func_no_trigger_4 x y z = x + y + z
;;

let func_no_trigger_5 ({ f1;
                         f2; _ } : my_record_fields) =
  String.length f2 + f1
;;

let func_no_trigger_6
  ({f1;f2;_} : my_record_fields)
  ({f3;_} : my_record_fields) =
  f1 + (String.length f2) + f3
;;

let func_no_trigger_7 x
  ({f1;f2;_} : my_record_fields) y
  ({f3;_} : my_record_fields) z =
  x + f1 + (String.length f2) + y + f3 + z
;;

let func_no_trigger_8 param1 param2
  ({f1; f2; _} : my_record_fields) =
  f1 + String.length f2 + param1 + param2
;;

let func_no_trigger_9
    x
    ({ f1; _ } : my_record_fields)
    y =
    x + f1 + y
;;
