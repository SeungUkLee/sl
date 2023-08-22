(* Recursion *)

let rec foo = fun f g n ->
  if n == 0
    then 0
    else foo (f g) (g f) (n - 1)
in
  foo (fun x -> x) (fun y -> y) 3

(* result : type error *)
