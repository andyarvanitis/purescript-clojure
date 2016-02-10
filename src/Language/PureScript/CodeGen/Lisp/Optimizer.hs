{-# LANGUAGE FlexibleContexts #-}

-- |
-- This module optimizes code in the simplified-Lisp intermediate representation.
--
-- The following optimizations are supported:
--
--  * Collapsing nested blocks
--
--  * Tail call elimination
--
--  * Inlining of (>>=) and ret for the Eff monad
--
--  * Removal of unnecessary thunks
--
--  * Eta conversion
--
--  * Inlining variables
--
--  * Inline Prelude.($), Prelude.(#), Prelude.(++), Prelude.(!!)
--
--  * Inlining primitive Lisp operators
--
module Language.PureScript.CodeGen.Lisp.Optimizer (optimize) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.CodeGen.Lisp.AST
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

import Language.PureScript.CodeGen.Lisp.Optimizer.Common
-- import Language.PureScript.CodeGen.Lisp.Optimizer.TCO
import Language.PureScript.CodeGen.Lisp.Optimizer.MagicDo
import Language.PureScript.CodeGen.Lisp.Optimizer.Inliner
import Language.PureScript.CodeGen.Lisp.Optimizer.Unused
import Language.PureScript.CodeGen.Lisp.Optimizer.Blocks

-- |
-- Apply a series of optimizer passes to simplified Lisp code
--
optimize :: (Monad m, MonadReader Options m, Applicative m, MonadSupply m) => Lisp -> m Lisp
optimize lisp = do
  noOpt <- asks optionsNoOptimizations
  if noOpt then return lisp else optimize' lisp

optimize' :: (Monad m, MonadReader Options m, Applicative m, MonadSupply m) => Lisp -> m Lisp
optimize' lisp = do
  opts <- ask
  untilFixedPoint (inlineFnComposition . applyAll
    [ collapseNestedBlocks
    , collapseNestedIfs
    -- , tco opts
    , magicDo opts
    , removeCodeAfterReturnStatements
    , removeUnusedArg
    , removeUndefinedApp
    , removeSafePrefix
    , unThunk
    , etaConvert
    , evaluateIifes
    , inlineVariables
    , inlineValues
    , inlineOperator (C.prelude, (C.$)) $ \f x -> LispApp f [x]
    , inlineOperator (C.dataFunction, C.apply) $ \f x -> LispApp f [x]
    , inlineOperator (C.prelude, (C.#)) $ \x f -> LispApp f [x]
    , inlineOperator (C.dataFunction, C.applyFlipped) $ \x f -> LispApp f [x]
    , inlineOperator (C.dataArrayUnsafe, C.unsafeIndex) $ flip LispIndexer
    , inlineCommonOperators
    ]) lisp

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'
