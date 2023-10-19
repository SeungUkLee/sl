{-# LANGUAGE OverloadedStrings #-}

module Program where

import           Interpreter
import           SLang

evalPgms :: [(TestEval Value, Value)]
evalPgms =
  [ (evaluate arithEx, VInt 7)
  , (evaluate equalEx, VBool False)
  , (evaluate letEx, VInt 1)
  , (evaluate letrecEx, VInt 6)
  , (evaluate ski3Ex, VInt 2)
  ]

arithEx, equalEx, letrecEx, ski3Ex, letEx :: Expr

-- | 1 + 2 * 3
arithEx = EOp Add (EConst (CInt 1)) (EOp Mul (EConst (CInt 2)) (EConst (CInt 3)))

-- | 1 == 2
equalEx = EOp Equal (EConst (CInt 1)) (EConst (CInt 2))

-- | let f = fun x -> x in f 1
letEx = ELet (LBVal "f" (EAbs "x" (EVar "x"))) (EApp (EVar "f") (EConst (CInt 1)))

-- | let rec fac n = if n == 0 then 1 else n * (fac (n - 1)) in fac 3
letrecEx =
  ELet
    (LBRec "fac" "n"
      (EIf
        (EOp Equal (EVar "n") (EConst (CInt 0)))
        (EConst (CInt 1))
        (EOp Mul (EVar "n") (EApp (EVar "fac") (EOp Sub (EVar "n") (EConst (CInt 1)))))))
    (EApp (EVar "fac") (EConst (CInt 3)))


{- | SKI Combinator
let i = fun x -> x in
let k = fun x -> fun y -> x in
let s = fun x -> fun y -> fun z -> (x z)(y z) in
s (k (s i)) (s (k k) i) 1
-}
ski3Ex =
  ELet
    (LBVal "i" (EAbs "x" (EVar "x")))
    (ELet
      (LBVal "k" (EAbs "x" (EAbs "y" (EVar "x"))))
      (ELet
        (LBVal "s" (EAbs "x" (EAbs "y" (EAbs "z" (EApp (EApp (EVar "x") (EVar "z")) (EApp (EVar "y") (EVar "z")))))))
        (EApp
          (EApp
            (EApp
              (EApp (EVar "s") (EApp (EVar "k") (EApp (EVar "s") (EVar "i"))))
              (EApp (EApp (EVar "s") (EApp (EVar "k") (EVar "k"))) (EVar "i")))
            (EConst (CInt 1)))
          (EAbs "x" (EOp Add (EVar "x") (EConst (CInt 1)))))))
