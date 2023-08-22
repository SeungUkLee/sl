(* equal (function) *)

let f = fun x -> x + 1 in
let g = fun x -> x + 2 in
f == g

(* result : type mismatch *)
