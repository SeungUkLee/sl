(* equal (function) *)

let f = fun x -> x + 1 in
let g = fun x -> true in
f == g

(* result : type error *)
