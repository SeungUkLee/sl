let i = fun x -> x in 
let k = fun x -> fun y -> x in 
let s = fun x -> fun y -> fun z -> (x z)(y z) in 
s (k (s i)) (s (k k) i) 1