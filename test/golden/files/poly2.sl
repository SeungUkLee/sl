(* polymorphic *)

let foo f g = fun x y -> f (g x) (g y) in foo

(* result : 
 * - : (a -> a -> b) -> (c -> a) -> c -> c -> b = <<function>> 
 *)
