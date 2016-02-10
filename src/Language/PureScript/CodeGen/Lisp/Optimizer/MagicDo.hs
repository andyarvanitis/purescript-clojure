-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
module Language.PureScript.CodeGen.Lisp.Optimizer.MagicDo (magicDo) where

import Data.List (nub)
import Data.Maybe (fromJust, isJust)

import Language.PureScript.CodeGen.Lisp.AST
import Language.PureScript.CodeGen.Lisp.Common
import Language.PureScript.CodeGen.Lisp.Optimizer.Common
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

magicDo :: Options -> Lisp -> Lisp
magicDo opts | optionsNoMagicDo opts = id
             | otherwise = inlineST . magicDo'

-- |
-- Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }
--
magicDo' :: Lisp -> Lisp
magicDo' = everywhereOnLisp undo . everywhereOnLispTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: Lisp -> Lisp
  -- Desugar pure & return
  convert (LispApp (LispApp pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (LispApp (LispApp bind [m]) [LispFunction Nothing [] (LispBlock lisp)]) | isBind bind =
    LispFunction (Just fnName) [] $ LispBlock (LispApp m [] : map applyReturns lisp )
  -- Desugar >>=
  convert (LispApp (LispApp bind [m]) [LispFunction Nothing [arg] (LispBlock lisp)]) | isBind bind =
    LispFunction (Just fnName) [] $ LispBlock (LispVariableIntroduction arg (Just (LispApp m [])) : map applyReturns lisp)
  -- Desugar untilE
  convert (LispApp (LispApp f [arg]) []) | isEffFunc C.untilE f =
    LispApp (LispFunction Nothing [] (LispBlock [ LispWhile (LispUnary Not (LispApp arg [])) (LispBlock []), LispReturn $ LispObjectLiteral []])) []
  -- Desugar whileE
  convert (LispApp (LispApp (LispApp f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    LispApp (LispFunction Nothing [] (LispBlock [ LispWhile (LispApp arg1 []) (LispBlock [ LispApp arg2 [] ]), LispReturn $ LispObjectLiteral []])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (LispApp fn [dict]) | isDict (C.eff, C.bindEffDictionary) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (LispApp fn [dict]) | isDict (C.eff, C.applicativeEffDictionary) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isFn' [(C.prelude, C.bind), (C.prelude, (C.>>=)), (C.controlBind, C.bind)]
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isFn' [(C.prelude, C.pure'), (C.prelude, C.return), (C.controlApplicative, C.pure')]
  -- Check if an expression represents a function in the Eff module
  isEffFunc name (LispAccessor name' (LispVar eff)) = eff == safeName C.eff && restoreName name == name'
  isEffFunc _ _ = False

  -- Remove __do function applications which remain after desugaring
  undo :: Lisp -> Lisp
  undo (LispReturn (LispApp (LispFunction (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

  applyReturns :: Lisp -> Lisp
  applyReturns (LispReturn ret) = LispReturn (LispApp ret [])
  applyReturns (LispBlock lisps) = LispBlock (map applyReturns lisps)
  applyReturns (LispWhile cond lisp) = LispWhile cond (applyReturns lisp)
  applyReturns (LispIfElse cond t f) = LispIfElse cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other

-- |
-- Inline functions in the ST module
--
inlineST :: Lisp -> Lisp
inlineST = everywhereOnLisp convertBlock
  where
  -- Look for runST blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same runST, only using { read, write, modify }STRef then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (LispApp f [arg]) | isSTFunc C.runST f =
    let refs = nub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in everywhereOnLisp (convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (LispApp f [arg]) | isSTFunc C.newSTRef f =
   LispFunction Nothing [] (LispBlock [LispReturn $ if agg then arg else LispObjectLiteral [(C.stRefValue, arg)]])
  convert agg (LispApp (LispApp f [ref]) []) | isSTFunc C.readSTRef f =
    if agg then ref else LispAccessor C.stRefValue ref
  convert agg (LispApp (LispApp (LispApp f [ref]) [arg]) []) | isSTFunc C.writeSTRef f =
    if agg then LispAssignment ref arg else LispAssignment (LispAccessor C.stRefValue ref) arg
  convert agg (LispApp (LispApp (LispApp f [ref]) [func]) []) | isSTFunc C.modifySTRef f =
    if agg then LispAssignment ref (LispApp func [ref]) else  LispAssignment (LispAccessor C.stRefValue ref) (LispApp func [LispAccessor C.stRefValue ref])
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (LispAccessor name' (LispVar st)) = st == safeName C.st && restoreName name == name'
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everythingOnLisp (++) isSTRef
    where
    isSTRef (LispVariableIntroduction ident (Just (LispApp (LispApp f [_]) []))) | isSTFunc C.newSTRef f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everythingOnLisp (++) isSTUsage
    where
    isSTUsage (LispApp (LispApp f [ref]) []) | isSTFunc C.readSTRef f = [ref]
    isSTUsage (LispApp (LispApp (LispApp f [ref]) [_]) []) | isSTFunc C.writeSTRef f || isSTFunc C.modifySTRef f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everythingOnLisp (++) isVar
    where
    isVar e@(LispVar v) | v == ref = [e]
    isVar _ = []
  -- Convert a Lisp value to a String if it is a LispVar
  toVar (LispVar v) = Just v
  toVar _ = Nothing
