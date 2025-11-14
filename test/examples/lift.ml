(* This test should trigger the linting rule for h, but not for g, as g is now long enough to be checked *)
let f x =
  let y = x + 1 in
  let g z =
    let a = y + 1 in
    let b = a + 2 in
    let c = b + 3 in
    let d = c + 4 in
    d + 5
  in
  let h a =
    let b = a + 1 in
    let c = b + 2 in
    let d = c + 3 in
    let e = d + 4 in
    e + 5
  in
  g y + h 10

(* This test should NOT trigger the linting rule, because g depends on y *)
let f_no_lift x =
  let y = x + 1 in
  let g z =
    let a = y + 1 in
    let b = a + 2 in
    let c = b + 3 in
    let d = c + 4 in
    d + 5
  in
  g 10

(* This test should trigger the linting rule, because h is recursive but does not capture any variables from the context of f_rec *)
let rec f_rec x =
  let h a =
    let b = a + 1 in
    let c = b + 2 in
    let d = c + 3 in
    let e = d + 4 in
    e + 5
  in
  if x = 0 then 1
  else h 10 + f_rec (x - 1)

(* This test should NOT trigger the linting rule, because h captures x *)
let rec f_rec_capture x =
    let h a =
        let b = a + 1 in
        let c = b + 2 in
        let d = c + 3 in
        let e = d + 4 in
        e + x
    in
    if x = 0 then 1
    else h 10 + f_rec_capture (x - 1)

(* This test should trigger the linting rule, because x is shadowed *)
let f_shadow x =
    let h a =
        let x = a + 1 in
        let b = x + 2 in
        let c = b + 3 in
        let d = c + 4 in
        d + 5
    in
    h 10 + x

(* This test should trigger the linting rule for can_be_lifted, but not for cannot_be_lifted *)
let rec outer_rec x =
    let cannot_be_lifted y =
        let a = y + x in
        let b = a + 2 in
        let c = b + 3 in
        let d = c + 4 in
        d + 5
    in
    let can_be_lifted z =
        let a = z + 1 in
        let b = a + 2 in
        let c = b + 3 in
        let d = c + 4 in
        d + 5
    in
    if x = 0 then 1
    else cannot_be_lifted 10 + can_be_lifted 20 + outer_rec (x - 1)
