-- |
-- Data types for the intermediate simplified-Lisp AST
--
module Language.PureScript.CodeGen.Lisp.AST where

import Prelude ()
import Prelude.Compat

import Control.Monad.Identity

import Language.PureScript.Comments
import Language.PureScript.Traversals

-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  -- |
  -- Constructor
  --
  | LispNew
  deriving (Show, Read, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division
  --
  | Divide
  -- |
  -- Remainder
  --
  | Modulus
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo
  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  -- |
  -- Bitwise right shift with zero-fill
  --
  | ZeroFillShiftRight
  deriving (Show, Read, Eq)

-- |
-- Data type for simplified Lisp expressions
--
data Lisp
  -- |
  -- A numeric literal
  --
  = LispNumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | LispStringLiteral String
  -- |
  -- A character literal
  --
  | LispCharLiteral Char
  -- |
  -- A boolean literal
  --
  | LispBooleanLiteral Bool
  -- |
  -- A unary operator application
  --
  | LispUnary UnaryOperator Lisp
  -- |
  -- A binary operator application
  --
  | LispBinary BinaryOperator Lisp Lisp
  -- |
  -- An array literal
  --
  | LispArrayLiteral [Lisp]
  -- |
  -- An array indexer expression
  --
  | LispIndexer Lisp Lisp
  -- |
  -- An object literal
  --
  | LispObjectLiteral [(String, Lisp)]
  -- |
  -- An object property accessor expression
  --
  | LispAccessor String Lisp
  -- |
  -- A function introduction (optional name, arguments, body)
  --
  | LispFunction (Maybe String) [String] Lisp
  -- |
  -- Function application
  --
  | LispApp Lisp [Lisp]
  -- |
  -- Variable
  --
  | LispVar String
  -- |
  -- Conditional expression
  --
  | LispConditional Lisp Lisp Lisp
  -- |
  -- A block of expressions in braces
  --
  | LispBlock [Lisp]
  -- |
  -- A variable introduction and optional initialization
  --
  | LispVariableIntroduction String (Maybe Lisp)
  -- |
  -- A variable assignment
  --
  | LispAssignment Lisp Lisp
  -- |
  -- While loop
  --
  | LispWhile Lisp Lisp
  -- |
  -- If-then-else statement
  --
  | LispIfElse Lisp Lisp (Maybe Lisp)
  -- |
  -- Return statement
  --
  | LispReturn Lisp
  -- |
  -- Throw statement
  --
  | LispThrow Lisp
  -- |
  -- InstanceOf test
  --
  | LispInstanceOf Lisp Lisp
  -- |
  -- Raw Lisp (generated when parsing fails for an inline foreign import declaration)
  --
  | LispRaw String
  -- |
  -- Commented Lisp
  --
  | LispComment [Comment] Lisp
  deriving (Show, Read, Eq)

--
-- Traversals
--

everywhereOnLisp :: (Lisp -> Lisp) -> Lisp -> Lisp
everywhereOnLisp f = go
  where
  go :: Lisp -> Lisp
  go (LispUnary op l) = f (LispUnary op (go l))
  go (LispBinary op l1 l2) = f (LispBinary op (go l1) (go l2))
  go (LispArrayLiteral ls) = f (LispArrayLiteral (map go ls))
  go (LispIndexer l1 l2) = f (LispIndexer (go l1) (go l2))
  go (LispObjectLiteral ls) = f (LispObjectLiteral (map (fmap go) ls))
  go (LispAccessor prop l) = f (LispAccessor prop (go l))
  go (LispFunction name args l) = f (LispFunction name args (go l))
  go (LispApp l ls) = f (LispApp (go l) (map go ls))
  go (LispConditional l1 l2 l3) = f (LispConditional (go l1) (go l2) (go l3))
  go (LispBlock ls) = f (LispBlock (map go ls))
  go (LispVariableIntroduction name l) = f (LispVariableIntroduction name (fmap go l))
  go (LispAssignment l1 l2) = f (LispAssignment (go l1) (go l2))
  go (LispWhile l1 l2) = f (LispWhile (go l1) (go l2))
  go (LispIfElse l1 l2 l3) = f (LispIfElse (go l1) (go l2) (fmap go l3))
  go (LispReturn ls) = f (LispReturn (go ls))
  go (LispThrow ls) = f (LispThrow (go ls))
  go (LispInstanceOf l1 l2) = f (LispInstanceOf (go l1) (go l2))
  go (LispComment com l) = f (LispComment com (go l))
  go other = f other

everywhereOnLispTopDown :: (Lisp -> Lisp) -> Lisp -> Lisp
everywhereOnLispTopDown f = runIdentity . everywhereOnLispTopDownM (Identity . f)

everywhereOnLispTopDownM :: (Applicative m, Monad m) => (Lisp -> m Lisp) -> Lisp -> m Lisp
everywhereOnLispTopDownM f = f >=> go
  where
  f' = f >=> go
  go (LispUnary op l) = LispUnary op <$> f' l
  go (LispBinary op l1 l2) = LispBinary op <$> f' l1 <*> f' l2
  go (LispArrayLiteral ls) = LispArrayLiteral <$> traverse f' ls
  go (LispIndexer l1 l2) = LispIndexer <$> f' l1 <*> f' l2
  go (LispObjectLiteral ls) = LispObjectLiteral <$> traverse (sndM f') ls
  go (LispAccessor prop l) = LispAccessor prop <$> f' l
  go (LispFunction name args l) = LispFunction name args <$> f' l
  go (LispApp l ls) = LispApp <$> f' l <*> traverse f' ls
  go (LispConditional l1 l2 l3) = LispConditional <$> f' l1 <*> f' l2 <*> f' l3
  go (LispBlock ls) = LispBlock <$> traverse f' ls
  go (LispVariableIntroduction name l) = LispVariableIntroduction name <$> traverse f' l
  go (LispAssignment l1 l2) = LispAssignment <$> f' l1 <*> f' l2
  go (LispWhile l1 l2) = LispWhile <$> f' l1 <*> f' l2
  go (LispIfElse l1 l2 l3) = LispIfElse <$> f' l1 <*> f' l2 <*> traverse f' l3
  go (LispReturn l) = LispReturn <$> f' l
  go (LispThrow l) = LispThrow <$> f' l
  go (LispInstanceOf l1 l2) = LispInstanceOf <$> f' l1 <*> f' l2
  go (LispComment com l) = LispComment com <$> f' l
  go other = f other

everythingOnLisp :: (r -> r -> r) -> (Lisp -> r) -> Lisp -> r
everythingOnLisp (<>) f = go
  where
  go l@(LispUnary _ l1) = f l <> go l1
  go l@(LispBinary _ l1 l2) = f l <> go l1 <> go l2
  go l@(LispArrayLiteral ls) = foldl (<>) (f l) (map go ls)
  go l@(LispIndexer l1 l2) = f l <> go l1 <> go l2
  go l@(LispObjectLiteral ls) = foldl (<>) (f l) (map (go . snd) ls)
  go l@(LispAccessor _ l1) = f l <> go l1
  go l@(LispFunction _ _ l1) = f l <> go l1
  go l@(LispApp l1 ls) = foldl (<>) (f l <> go l1) (map go ls)
  go l@(LispConditional l1 l2 l3) = f l <> go l1 <> go l2 <> go l3
  go l@(LispBlock ls) = foldl (<>) (f l) (map go ls)
  go l@(LispVariableIntroduction _ (Just l1)) = f l <> go l1
  go l@(LispAssignment l1 l2) = f l <> go l1 <> go l2
  go l@(LispWhile l1 l2) = f l <> go l1 <> go l2
  go l@(LispIfElse l1 l2 Nothing) = f l <> go l1 <> go l2
  go l@(LispIfElse l1 l2 (Just l3)) = f l <> go l1 <> go l2 <> go l3
  go l@(LispReturn l1) = f l <> go l1
  go l@(LispThrow l1) = f l <> go l1
  go l@(LispInstanceOf l1 l2) = f l <> go l1 <> go l2
  go l@(LispComment _ l1) = f l <> go l1
  go other = f other
