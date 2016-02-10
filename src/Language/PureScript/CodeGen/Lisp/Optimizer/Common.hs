-- |
-- Common functions used by the various optimizer phases
--
module Language.PureScript.CodeGen.Lisp.Optimizer.Common where

import Data.Maybe (fromMaybe)

import Language.PureScript.Crash
import Language.PureScript.CodeGen.Lisp.AST
import Language.PureScript.CodeGen.Lisp.Common
import Language.PureScript.Names

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

replaceIdent :: String -> Lisp -> Lisp -> Lisp
replaceIdent var1 lisp = everywhereOnLisp replace
  where
  replace (LispVar var2) | var1 == var2 = lisp
  replace other = other

replaceIdents :: [(String, Lisp)] -> Lisp -> Lisp
replaceIdents vars = everywhereOnLisp replace
  where
  replace v@(LispVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: String -> Lisp -> Bool
isReassigned var1 = everythingOnLisp (||) check
  where
  check :: Lisp -> Bool
  check (LispFunction _ args _) | var1 `elem` args = True
  check (LispVariableIntroduction arg _) | var1 == arg = True
  check (LispAssignment (LispVar arg) _) | var1 == arg = True
  check _ = False

isRebound :: Lisp -> Lisp -> Bool
isRebound lisp d = any (\v -> isReassigned v d || isUpdated v d) (everythingOnLisp (++) variablesOf lisp)
  where
  variablesOf (LispVar var) = [var]
  variablesOf _ = []

isUsed :: String -> Lisp -> Bool
isUsed var1 = everythingOnLisp (||) check
  where
  check :: Lisp -> Bool
  check (LispVar var2) | var1 == var2 = True
  check (LispAssignment target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: Lisp -> String
targetVariable (LispVar var) = var
targetVariable (LispAccessor _ tgt) = targetVariable tgt
targetVariable (LispIndexer _ tgt) = targetVariable tgt
targetVariable _ = internalError "Invalid argument to targetVariable"

isUpdated :: String -> Lisp -> Bool
isUpdated var1 = everythingOnLisp (||) check
  where
  check :: Lisp -> Bool
  check (LispAssignment target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([Lisp] -> [Lisp]) -> Lisp -> Lisp
removeFromBlock go (LispBlock sts) = LispBlock (go sts)
removeFromBlock _  lisp = lisp

isFn :: (String, String) -> Lisp -> Bool
isFn (moduleName, fnName) (LispAccessor x (LispVar y)) = x == safeName fnName && y == restoreName moduleName
isFn (moduleName, fnName) (LispAccessor x (LispVar y)) = x == fnName && y == moduleName
isFn (moduleName, fnName) (LispIndexer (LispStringLiteral x) (LispVar y)) = x == fnName && y == restoreName moduleName
isFn _ _ = False

isFn' :: [(String, String)] -> Lisp -> Bool
isFn' xs lisp = any (`isFn` lisp) xs

isDict :: (String, String) -> Lisp -> Bool
isDict (moduleName, dictName) (LispAccessor x (LispVar y)) = x == safeName dictName && y == restoreName moduleName
isDict _ _ = False

isDict' :: [(String, String)] -> Lisp -> Bool
isDict' xs lisp = any (`isDict` lisp) xs

restoreName :: String -> String
restoreName s = map (\c -> if c == '_' then '.' else c) s
