{-# LANGUAGE OverloadedStrings #-}

module Program
  ( parsePgm
  , parseArith
  , parseFunc
  , parseIfThenElse
  , parseLet
  , parseLetRec
  ) where

import           Interpreter
import           SLang
import qualified Data.Text as T

parsePgm :: T.Text -> TestParser Expr
parsePgm = parse "(input)"

parseArith :: [(TestParser Expr, Expr)]
parseArith =
  [ (parse "(input)" "1 + 2 * 3", EOp Add (EConst (CInt 1)) (EOp Mul (EConst (CInt 2)) (EConst (CInt 3))))
  , (parse "(input)" "(1 + 2) * 3", EOp Mul (EOp Add (EConst (CInt 1)) (EConst (CInt 2))) (EConst (CInt 3)))
  ]

parseIfThenElse :: [(TestParser Expr, Expr)]
parseIfThenElse =
  [ (parse "(input)" "if x == 1 then 1 else 2", EIf (EOp Equal (EVar "x") (EConst (CInt 1))) (EConst (CInt 1)) (EConst (CInt 2)))
  , (parse "(input)" "if true then 1 else 2", EIf (EConst (CBool True)) (EConst (CInt 1)) (EConst (CInt 2)))
  ]

parseFunc :: [(TestParser Expr, Expr)]
parseFunc =
  [ (parse "(input)" "fun x -> x", EAbs "x" (EVar "x"))
  , (parse "(input)" "fun a b -> a + b", EAbs "a" (EAbs "b" (EOp Add (EVar "a") (EVar "b"))))
  , (parse "(input)" "fun a -> fun b -> a + b", EAbs "a" (EAbs "b" (EOp Add (EVar "a") (EVar "b"))))
  ]

parseLet :: [(TestParser Expr, Expr)]
parseLet =
  [ (parse "(input)" "let x = 1 in x", ELet (LBVal "x" (EConst (CInt 1))) (EVar "x"))
  , (parse "(input)" "let f a b = a + b in f 1 2", ELet (LBVal "f" (EAbs "a" (EAbs "b" (EOp Add (EVar "a") (EVar "b"))))) (EApp (EApp (EVar "f") (EConst (CInt 1))) (EConst (CInt 2))))
  , (parse "(input)" "let f a = fun b -> a + b in f 1 2", ELet (LBVal "f" (EAbs "a" (EAbs "b" (EOp Add (EVar "a") (EVar "b"))))) (EApp (EApp (EVar "f") (EConst (CInt 1))) (EConst (CInt 2))))
  , (parse "(input)" "let f = fun a -> fun b -> a + b in f 1 2", ELet (LBVal "f" (EAbs "a" (EAbs "b" (EOp Add (EVar "a") (EVar "b"))))) (EApp (EApp (EVar "f") (EConst (CInt 1))) (EConst (CInt 2))))
  , (parse "(input)" "let f = fun a b -> a + b in f 1 2", ELet (LBVal "f" (EAbs "a" (EAbs "b" (EOp Add (EVar "a") (EVar "b"))))) (EApp (EApp (EVar "f") (EConst (CInt 1))) (EConst (CInt 2))))
  ]

parseLetRec :: [(TestParser Expr, Expr)]
parseLetRec =
  [ (parse "(input)" "let rec f = fun n -> if n == 0 then 1 else n * (fac (n-1)) in fac 3", ELet (LBRec "f" "n" (EIf (EOp Equal (EVar "n") (EConst (CInt 0))) (EConst (CInt 1)) (EOp Mul (EVar "n") (EApp (EVar "fac") (EOp Sub (EVar "n") (EConst (CInt 1))))))) (EApp (EVar "fac") (EConst (CInt 3))))
  , (parse "(input)" "let rec f n = if n == 0 then 1 else n * (fac (n-1)) in fac 3", ELet (LBRec "f" "n" (EIf (EOp Equal (EVar "n") (EConst (CInt 0))) (EConst (CInt 1)) (EOp Mul (EVar "n") (EApp (EVar "fac") (EOp Sub (EVar "n") (EConst (CInt 1))))))) (EApp (EVar "fac") (EConst (CInt 3))))
  , (parse "(input)" "let rec f a b = a + b in f 1 2", ELet (LBRec "f" "a" (EAbs "b" (EOp Add (EVar "a") (EVar "b")))) (EApp (EApp (EVar "f") (EConst (CInt 1))) (EConst (CInt 2))))
  ]
