-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Lisp.Optimizer.Blocks
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Optimizer steps for simplifying Lisp blocks
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Lisp.Optimizer.Blocks
  ( collapseNestedBlocks
  , collapseNestedIfs
  ) where

import Language.PureScript.CodeGen.Lisp.AST

-- |
-- Collapse blocks which appear nested directly below another block
--
collapseNestedBlocks :: Lisp -> Lisp
collapseNestedBlocks = everywhereOnLisp collapse
  where
  collapse :: Lisp -> Lisp
  collapse (LispBlock sts) = LispBlock (concatMap go sts)
  collapse lisp = lisp
  go :: Lisp -> [Lisp]
  go (LispBlock sts) = sts
  go s = [s]

collapseNestedIfs :: Lisp -> Lisp
collapseNestedIfs = everywhereOnLisp collapse
  where
  collapse :: Lisp -> Lisp
  collapse (LispIfElse cond1 (LispBlock [LispIfElse cond2 body Nothing]) Nothing) =
      LispIfElse (LispBinary And cond1 cond2) body Nothing
  collapse lisp = lisp
