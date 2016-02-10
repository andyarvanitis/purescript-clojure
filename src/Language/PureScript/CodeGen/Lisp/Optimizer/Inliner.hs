-- |
-- This module provides basic inlining capabilities
--
module Language.PureScript.CodeGen.Lisp.Optimizer.Inliner
  ( inlineVariables
  , inlineValues
  , inlineOperator
  , inlineCommonOperators
  , inlineFnComposition
  , etaConvert
  , unThunk
  , evaluateIifes
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)
import Data.Maybe (fromMaybe)

import Language.PureScript.CodeGen.Lisp.AST
import Language.PureScript.CodeGen.Lisp.Common
import Language.PureScript.Names
import Language.PureScript.CodeGen.Lisp.Optimizer.Common
import qualified Language.PureScript.Constants as C

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: Lisp -> Bool
shouldInline (LispVar _) = True
shouldInline (LispNumericLiteral _) = True
shouldInline (LispStringLiteral _) = True
shouldInline (LispBooleanLiteral _) = True
shouldInline (LispAccessor _ val) = shouldInline val
shouldInline (LispIndexer index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: Lisp -> Lisp
etaConvert = everywhereOnLisp convert
  where
  convert :: Lisp -> Lisp
  convert (LispBlock [LispReturn (LispApp (LispFunction Nothing idents block@(LispBlock body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map LispVar idents)) &&
      not (any (`isRebound` block) args)
      = LispBlock (map (replaceIdents (zip idents args)) body)
  convert (LispFunction Nothing [] (LispBlock [LispReturn (LispApp fn [])])) = fn
  convert lisp = lisp

unThunk :: Lisp -> Lisp
unThunk = everywhereOnLisp convert
  where
  convert :: Lisp -> Lisp
  convert (LispBlock []) = LispBlock []
  convert (LispBlock lisps) =
    case last lisps of
      LispReturn (LispApp (LispFunction Nothing [] (LispBlock body)) []) -> LispBlock $ init lisps ++ body
      _ -> LispBlock lisps
  convert lisp = lisp

evaluateIifes :: Lisp -> Lisp
evaluateIifes = everywhereOnLisp convert
  where
  convert :: Lisp -> Lisp
  convert (LispApp (LispFunction Nothing [] (LispBlock [LispReturn ret])) []) = ret
  convert lisp = lisp

inlineVariables :: Lisp -> Lisp
inlineVariables = everywhereOnLisp $ removeFromBlock go
  where
  go :: [Lisp] -> [Lisp]
  go [] = []
  go (LispVariableIntroduction var (Just lisp) : sts)
    | shouldInline lisp && not (any (isReassigned var) sts) && not (any (isRebound lisp) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var lisp) sts)
  go (s:sts) = s : go sts

inlineValues :: Lisp -> Lisp
inlineValues = everywhereOnLisp convert
  where
  convert :: Lisp -> Lisp
  convert (LispApp fn [dict])
    | isDict' (semiringNumber ++ semiringInt) dict && isFn' fnZero fn = LispNumericLiteral (Left 0)
    | isDict' (semiringNumber ++ semiringInt) dict && isFn' fnOne fn = LispNumericLiteral (Left 1)
    | isDict' boundedBoolean dict && isFn' fnBottom fn = LispBooleanLiteral False
    | isDict' boundedBoolean dict && isFn' fnTop fn = LispBooleanLiteral True
  convert (LispApp (LispApp (LispApp fn [dict]) [x]) [y])
    | isDict' semiringInt dict && isFn' fnAdd fn = LispBinary Add x y
    | isDict' semiringInt dict && isFn' fnMultiply fn = LispBinary Multiply x y
    | isDict' moduloSemiringInt dict && isFn' fnDivide fn = LispBinary Divide x y
    | isDict' ringInt dict && isFn' fnSubtract fn = LispBinary Subtract x y
  convert other = other
  fnZero = [(C.prelude, C.zero), (C.dataSemiring, C.zero)]
  fnOne = [(C.prelude, C.one), (C.dataSemiring, C.one)]
  fnBottom = [(C.prelude, C.bottom), (C.dataBounded, C.bottom)]
  fnTop = [(C.prelude, C.top), (C.dataBounded, C.top)]
  fnAdd = [(C.prelude, (C.+)), (C.prelude, (C.add)), (C.dataSemiring, (C.+)), (C.dataSemiring, (C.add))]
  fnDivide = [(C.prelude, (C./)), (C.prelude, (C.div)), (C.dataModuloSemiring, C.div)]
  fnMultiply = [(C.prelude, (C.*)), (C.prelude, (C.mul)), (C.dataSemiring, (C.*)), (C.dataSemiring, (C.mul))]
  fnSubtract = [(C.prelude, (C.-)), (C.prelude, C.sub), (C.dataRing, C.sub)]

inlineOperator :: (String, String) -> (Lisp -> Lisp -> Lisp) -> Lisp -> Lisp
inlineOperator (m, op) f = everywhereOnLisp convert
  where
  convert :: Lisp -> Lisp
  convert (LispApp (LispApp op' [x]) [y]) | isOp op' = f x y
  convert other = other
  isOp (LispAccessor longForm (LispVar m')) = m == m' && longForm == identToLisp (Op op)
  isOp (LispIndexer (LispStringLiteral op') (LispVar m')) = m == m' && op == op'
  isOp _ = False

inlineCommonOperators :: Lisp -> Lisp
inlineCommonOperators = applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate
  , binary ringInt opSub Subtract
  , unary  ringInt opNegate Negate

  , binary moduloSemiringNumber opDiv Divide
  , binary moduloSemiringInt opMod Modulus

  , binary eqNumber opEq EqualTo
  , binary eqNumber opNotEq NotEqualTo
  , binary eqInt opEq EqualTo
  , binary eqInt opNotEq NotEqualTo
  , binary eqString opEq EqualTo
  , binary eqString opNotEq NotEqualTo
  , binary eqChar opEq EqualTo
  , binary eqChar opNotEq NotEqualTo
  , binary eqBoolean opEq EqualTo
  , binary eqBoolean opNotEq NotEqualTo

  , binary ordBoolean opLessThan LessThan
  , binary ordBoolean opLessThanOrEq LessThanOrEqualTo
  , binary ordBoolean opGreaterThan GreaterThan
  , binary ordBoolean opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordChar opLessThan LessThan
  , binary ordChar opLessThanOrEq LessThanOrEqualTo
  , binary ordChar opGreaterThan GreaterThan
  , binary ordChar opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordInt opLessThan LessThan
  , binary ordInt opLessThanOrEq LessThanOrEqualTo
  , binary ordInt opGreaterThan GreaterThan
  , binary ordInt opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordNumber opLessThan LessThan
  , binary ordNumber opLessThanOrEq LessThanOrEqualTo
  , binary ordNumber opGreaterThan GreaterThan
  , binary ordNumber opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordString opLessThan LessThan
  , binary ordString opLessThanOrEq LessThanOrEqualTo
  , binary ordString opGreaterThan GreaterThan
  , binary ordString opGreaterThanOrEq GreaterThanOrEqualTo

  , binary semigroupString opAppend Add

  , binary booleanAlgebraBoolean opConj And
  , binary booleanAlgebraBoolean opDisj Or
  , unary  booleanAlgebraBoolean opNot Not

  , binary' C.dataIntBits (C..|.) BitwiseOr
  , binary' C.dataIntBits (C..&.) BitwiseAnd
  , binary' C.dataIntBits (C..^.) BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement BitwiseNot
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: [(String, String)] -> [(String, String)] -> BinaryOperator -> Lisp -> Lisp
  binary dict fns op = everywhereOnLisp convert
    where
    convert :: Lisp -> Lisp
    convert (LispApp (LispApp (LispApp fn [dict']) [x]) [y]) | isDict' dict dict' && isFn' fns fn = LispBinary op x y
    convert other = other
  binary' :: String -> String -> BinaryOperator -> Lisp -> Lisp
  binary' moduleName opString op = everywhereOnLisp convert
    where
    convert :: Lisp -> Lisp
    convert (LispApp (LispApp fn [x]) [y]) | isFn (moduleName, opString) fn = LispBinary op x y
    convert other = other
  unary :: [(String, String)] -> [(String, String)] -> UnaryOperator -> Lisp -> Lisp
  unary dicts fns op = everywhereOnLisp convert
    where
    convert :: Lisp -> Lisp
    convert (LispApp (LispApp fn [dict']) [x]) | isDict' dicts dict' && isFn' fns fn = LispUnary op x
    convert other = other
  unary' :: String -> String -> UnaryOperator -> Lisp -> Lisp
  unary' moduleName fnName op = everywhereOnLisp convert
    where
    convert :: Lisp -> Lisp
    convert (LispApp fn [x]) | isFn (moduleName, fnName) fn = LispUnary op x
    convert other = other
  mkFn :: Int -> Lisp -> Lisp
  mkFn 0 = everywhereOnLisp convert
    where
    convert :: Lisp -> Lisp
    convert (LispApp mkFnN [LispFunction Nothing [_] (LispBlock lisp)]) | isNFn C.mkFn 0 mkFnN =
      LispFunction Nothing [] (LispBlock lisp)
    convert other = other
  mkFn n = everywhereOnLisp convert
    where
    convert :: Lisp -> Lisp
    convert orig@(LispApp mkFnN [fn]) | isNFn C.mkFn n mkFnN =
      case collectArgs n [] fn of
        Just (args, lisp) -> LispFunction Nothing args (LispBlock lisp)
        Nothing -> orig
    convert other = other
    collectArgs :: Int -> [String] -> Lisp -> Maybe ([String], [Lisp])
    collectArgs 1 acc (LispFunction Nothing [oneArg] (LispBlock lisp)) | length acc == n - 1 = Just (reverse (oneArg : acc), lisp)
    collectArgs m acc (LispFunction Nothing [oneArg] (LispBlock [LispReturn ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: String -> Int -> Lisp -> Bool
  isNFn prefix n (LispVar name) = name == (prefix ++ show n)
  isNFn prefix n (LispAccessor name (LispVar dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
  isNFn _ _ _ = False

  runFn :: Int -> Lisp -> Lisp
  runFn n = everywhereOnLisp convert
    where
    convert :: Lisp -> Lisp
    convert lisp = fromMaybe lisp $ go n [] lisp

    go :: Int -> [Lisp] -> Lisp -> Maybe Lisp
    go 0 acc (LispApp runFnN [fn]) | isNFn C.runFn n runFnN && length acc == n = Just (LispApp fn acc)
    go m acc (LispApp lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: (Applicative m, MonadSupply m) => Lisp -> m Lisp
inlineFnComposition = everywhereOnLispTopDownM convert
  where
  convert :: (MonadSupply m) => Lisp -> m Lisp
  convert (LispApp (LispApp (LispApp (LispApp fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ LispApp x [LispApp y [z]]
    | isFnComposeFlipped dict' fn = return $ LispApp y [LispApp x [z]]
  convert (LispApp (LispApp (LispApp fn [dict']) [x]) [y])
    | isFnCompose dict' fn = do
        arg <- freshName
        return $ LispFunction Nothing [arg] (LispBlock [LispReturn $ LispApp x [LispApp y [LispVar arg]]])
    | isFnComposeFlipped dict' fn = do
        arg <- freshName
        return $ LispFunction Nothing [arg] (LispBlock [LispReturn $ LispApp y [LispApp x [LispVar arg]]])
  convert other = return other
  isFnCompose :: Lisp -> Lisp -> Bool
  isFnCompose dict' fn = isDict' semigroupoidFn dict' && isFn' fnCompose fn
  isFnComposeFlipped :: Lisp -> Lisp -> Bool
  isFnComposeFlipped dict' fn = isDict' semigroupoidFn dict' && isFn' fnComposeFlipped fn
  fnCompose :: [(String, String)]
  fnCompose = [(C.prelude, C.compose), (C.prelude, (C.<<<)), (C.controlSemigroupoid, C.compose)]
  fnComposeFlipped :: [(String, String)]
  fnComposeFlipped = [(C.prelude, (C.>>>)), (C.controlSemigroupoid, C.composeFlipped)]

semiringNumber :: [(String, String)]
semiringNumber = [(C.prelude, C.semiringNumber), (C.dataSemiring, C.semiringNumber)]

semiringInt :: [(String, String)]
semiringInt = [(C.prelude, C.semiringInt), (C.dataSemiring, C.semiringInt)]

ringNumber :: [(String, String)]
ringNumber = [(C.prelude, C.ringNumber), (C.dataRing, C.ringNumber)]

ringInt :: [(String, String)]
ringInt = [(C.prelude, C.ringInt), (C.dataRing, C.ringInt)]

moduloSemiringNumber :: [(String, String)]
moduloSemiringNumber = [(C.prelude, C.moduloSemiringNumber), (C.dataModuloSemiring, C.moduloSemiringNumber)]

moduloSemiringInt :: [(String, String)]
moduloSemiringInt = [(C.prelude, C.moduloSemiringInt), (C.dataModuloSemiring, C.moduloSemiringInt)]

eqNumber :: [(String, String)]
eqNumber = [(C.prelude, C.eqNumber), (C.dataEq, C.eqNumber)]

eqInt :: [(String, String)]
eqInt = [(C.prelude, C.eqInt), (C.dataEq, C.eqInt)]

eqString :: [(String, String)]
eqString = [(C.prelude, C.eqString), (C.dataEq, C.eqString)]

eqChar :: [(String, String)]
eqChar = [(C.prelude, C.eqChar), (C.dataEq, C.eqChar)]

eqBoolean :: [(String, String)]
eqBoolean = [(C.prelude, C.eqBoolean), (C.dataEq, C.eqBoolean)]

ordBoolean :: [(String, String)]
ordBoolean = [(C.prelude, C.ordBoolean), (C.dataOrd, C.ordBoolean)]

ordNumber :: [(String, String)]
ordNumber = [(C.prelude, C.ordNumber), (C.dataOrd, C.ordNumber)]

ordInt :: [(String, String)]
ordInt = [(C.prelude, C.ordInt), (C.dataOrd, C.ordInt)]

ordString :: [(String, String)]
ordString = [(C.prelude, C.ordString), (C.dataOrd, C.ordString)]

ordChar :: [(String, String)]
ordChar = [(C.prelude, C.ordChar), (C.dataOrd, C.ordChar)]

semigroupString :: [(String, String)]
semigroupString = [(C.prelude, C.semigroupString), (C.dataSemigroup, C.semigroupString)]

boundedBoolean :: [(String, String)]
boundedBoolean = [(C.prelude, C.boundedBoolean), (C.dataBounded, C.boundedBoolean)]

booleanAlgebraBoolean :: [(String, String)]
booleanAlgebraBoolean = [(C.prelude, C.booleanAlgebraBoolean), (C.dataBooleanAlgebra, C.booleanAlgebraBoolean)]

semigroupoidFn :: [(String, String)]
semigroupoidFn = [(C.prelude, C.semigroupoidFn), (C.controlSemigroupoid, C.semigroupoidFn)]

opAdd :: [(String, String)]
opAdd = [(C.prelude, (C.+)), (C.prelude, C.add), (C.dataSemiring, C.add)]

opMul :: [(String, String)]
opMul = [(C.prelude, (C.*)), (C.prelude, C.mul), (C.dataSemiring, C.mul)]

opEq :: [(String, String)]
opEq = [(C.prelude, (C.==)), (C.prelude, C.eq), (C.dataEq, C.eq)]

opNotEq :: [(String, String)]
opNotEq = [(C.prelude, (C./=)), (C.dataEq, C.notEq)]

opLessThan :: [(String, String)]
opLessThan = [(C.prelude, (C.<)), (C.dataOrd, C.lessThan)]

opLessThanOrEq :: [(String, String)]
opLessThanOrEq = [(C.prelude, (C.<=)), (C.dataOrd, C.lessThanOrEq)]

opGreaterThan :: [(String, String)]
opGreaterThan = [(C.prelude, (C.>)), (C.dataOrd, C.greaterThan)]

opGreaterThanOrEq :: [(String, String)]
opGreaterThanOrEq = [(C.prelude, (C.>=)), (C.dataOrd, C.greaterThanOrEq)]

opAppend :: [(String, String)]
opAppend = [(C.prelude, (C.<>)), (C.prelude, (C.++)), (C.prelude, C.append), (C.dataSemigroup, C.append)]

opSub :: [(String, String)]
opSub = [(C.prelude, (C.-)), (C.prelude, C.sub), (C.dataRing, C.sub)]

opNegate :: [(String, String)]
opNegate = [(C.prelude, C.negate), (C.dataRing, C.negate)]

opDiv :: [(String, String)]
opDiv = [(C.prelude, (C./)), (C.prelude, C.div), (C.dataModuloSemiring, C.div)]

opMod :: [(String, String)]
opMod = [(C.prelude, C.mod), (C.dataModuloSemiring, C.mod)]

opConj :: [(String, String)]
opConj = [(C.prelude, (C.&&)), (C.prelude, C.conj), (C.dataBooleanAlgebra, C.conj)]

opDisj :: [(String, String)]
opDisj = [(C.prelude, (C.||)), (C.prelude, C.disj), (C.dataBooleanAlgebra, C.disj)]

opNot :: [(String, String)]
opNot = [(C.prelude, C.not), (C.dataBooleanAlgebra, C.not)]
