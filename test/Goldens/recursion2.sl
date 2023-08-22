(* Recursion *)

let rec iter = fun f n a ->
  if n == 0
    then 0
    else iter f (n - 1) (f a)
in
  iter (fun x -> x + 1) 10 false

(* result : type error *)
