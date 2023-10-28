(* SKI Combinators *)

let i = fun x -> x in 
let k = fun x y -> x in 
let s = fun x y z -> (x z)(y z) in 
s (k (s i)) (s (k k) i) 1 (fun x -> true)

(* result :
 * - : bool = true
 *)