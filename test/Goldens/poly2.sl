(* polymorphic *)

let foo f g = fun x y -> f (g x) (g y) in foo
