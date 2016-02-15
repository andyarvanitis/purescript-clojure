-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Lisp
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for the Lisp AST
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Lisp (
    prettyPrintLisp
) where

import Prelude ()
import Prelude.Compat

import Data.List hiding (concat, concatMap)
import Data.Maybe (fromMaybe)

import Control.Arrow ((<+>))
import Control.Monad.State hiding (sequence)
import Control.PatternArrows
import qualified Control.Arrow as A

import Language.PureScript.Crash
import Language.PureScript.CodeGen.Lisp.AST
import Language.PureScript.CodeGen.Lisp.Common
import Language.PureScript.Pretty.Common
import Language.PureScript.Comments

import Numeric

literals :: Pattern PrinterState Lisp String
literals = mkPattern' match
  where
  match :: Lisp -> StateT PrinterState Maybe String
  match (LispNumericLiteral n) = return $ either show show n
  match (LispStringLiteral s) = return $ string s
  match (LispCharLiteral c) = return $ '\\' : [c]
  match (LispBooleanLiteral True) = return "true"
  match (LispBooleanLiteral False) = return "false"
  match (LispArrayLiteral []) = return "[]"
  match (LispArrayLiteral xs) = concat <$> sequence
    [ return "["
    , intercalate " " <$> forM xs prettyPrintLisp'
    , return "]"
    ]
  match (LispObjectLiteral []) = return "{}"
  match (LispObjectLiteral ps) = concat <$> sequence
    [ return "{\n"
    , withIndent $ do
        lisps <- forM ps $ \(key, value) ->
                             fmap ((safeName (':':key) ++ " ") ++) . prettyPrintLisp' $ value
        indentString <- currentIndent
        return $ intercalate ", \n" $ map (indentString ++) lisps
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (LispBlock [ret@LispObjectLiteral{}]) = withIndent $ prettyPrintLisp' ret
  match (LispBlock sts@(_:_:_))
    | all isIfReturn (init sts)
    = concat <$> sequence
    [ return "(cond \n"
    , withIndent $ prettyStatements (compact <$> sts)
    , return "\n"
    , currentIndent
    , return ")"
    ]
    where
    isIfReturn :: Lisp -> Bool
    isIfReturn (LispIfElse _ (LispBlock [LispReturn{}]) Nothing) = True
    isIfReturn _ = False
    compact :: Lisp -> Lisp
    compact (LispIfElse cond (LispBlock [LispReturn ret]) Nothing) =
      LispVar $ prettyPrintLisp1 cond ++ " " ++ prettyPrintLisp1 ret
    compact lisp' = LispVar $ ":else " ++ prettyPrintLisp1 lisp'
  match (LispBlock sts) = concat <$> sequence
    (case compacted of
      [sts'] -> [ return "\n"
                , withIndent $ prettyStatements [sts']
                ]
      sts'   -> [ return "(do \n"
                , withIndent $ prettyStatements sts'
                , return "\n"
                , currentIndent
                , return ")"
                ])
    where
    compacted = compact sts
    compact :: [Lisp] -> [Lisp]
    compact ((LispIfElse cond block@(LispBlock [LispReturn{}]) Nothing) : sts') =
      [LispIfElse cond block (Just . LispBlock $ compact sts')]
    compact ((LispVariableIntroduction var (Just val)) : sts') =
      [LispApp (LispVar "let") (LispArrayLiteral ([LispVar var, val] ++ vars') : [LispBlock (compact others)])]
      where
      (vars, others) = span isVarIntro sts'
      vars' :: [Lisp]
      vars' = concatMap varval vars
      isVarIntro :: Lisp -> Bool
      isVarIntro LispVariableIntroduction{} = True
      isVarIntro _ = False
      varval :: Lisp -> [Lisp]
      varval (LispVariableIntroduction var' (Just val')) = [LispVar var', val']
      varval _ = []
    compact (st':sts') = st' : compact sts'
    compact [] = []
  match (LispVar ident) = return ident
  match (LispVariableIntroduction ident (Just (LispFunction Nothing args body))) =
    prettyPrintLisp' (LispFunction (Just ident) args body)
  match (LispVariableIntroduction ident value) = concat <$> sequence
    [ return "(def "
    , return ident
    , maybe (return "") (fmap (" " ++) . prettyPrintLisp') value
    , return ")"
    ]
  match (LispAssignment target value) = concat <$> sequence
    [ prettyPrintLisp' target
    , return " = "
    , prettyPrintLisp' value
    ]
  match (LispWhile cond sts) = concat <$> sequence
    [ return "while ("
    , prettyPrintLisp' cond
    , return ") "
    , prettyPrintLisp' sts
    ]
  match (LispIfElse cond thens elses) = concat <$> sequence
    [ return "(if "
    , prettyPrintLisp' cond
    , return " "
    , prettyPrintLisp' thens
    , maybe (return "") (fmap (" " ++) . prettyPrintLisp') elses
    , return ")"
    ]
  match (LispReturn value) = concat <$> sequence
    [ prettyPrintLisp' value
    ]
  match (LispThrow value) = concat <$> sequence
    [ return "(throw (Exception. "
    , prettyPrintLisp' value
    , return "))"
    ]
  match (LispComment com lisp) = fmap concat $ sequence $
    [ return "\n"
    , currentIndent
    , return ";;\n"
    ] ++
    map asLine (concatMap commentLines com) ++
    [ currentIndent
    , return ";;\n"
    , currentIndent
    , prettyPrintLisp' lisp
    ]
    where
    commentLines :: Comment -> [String]
    commentLines (LineComment s) = [s]
    commentLines (BlockComment s) = lines s

    asLine :: String -> StateT PrinterState Maybe String
    asLine s = do
      i <- currentIndent
      return $ i ++ ";; " ++ removeComments s ++ "\n"

    removeComments :: String -> String
    removeComments ('*' : '/' : s) = removeComments s
    removeComments (c : s) = c : removeComments s

    removeComments [] = []
  match (LispRaw lisp) = return lisp
  match _ = mzero

string :: String -> String
string s = '"' : concatMap encodeChar s ++ "\""
  where
  encodeChar :: Char -> String
  encodeChar '\b' = "\\b"
  encodeChar '\t' = "\\t"
  encodeChar '\n' = "\\n"
  encodeChar '\v' = "\\v"
  encodeChar '\f' = "\\f"
  encodeChar '\r' = "\\r"
  encodeChar '"'  = "\\\""
  encodeChar '\\' = "\\\\"
  encodeChar c | fromEnum c > 0xFFFF = "\\u" ++ showHex highSurrogate ("\\u" ++ showHex lowSurrogate "")
    where
    (h, l) = divMod (fromEnum c - 0x10000) 0x400
    highSurrogate = h + 0xD800
    lowSurrogate = l + 0xDC00
  encodeChar c | fromEnum c > 0xFFF = "\\u" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0xFF = "\\u0" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c < 0x10 = "\\x0" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0x7E || fromEnum c < 0x20 = "\\x" ++ showHex (fromEnum c) ""
  encodeChar c = [c]

conditional :: Pattern PrinterState Lisp ((Lisp, Lisp), Lisp)
conditional = mkPattern match
  where
  match (LispConditional cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern PrinterState Lisp (String, Lisp)
accessor = mkPattern match
  where
  match (LispAccessor prop val) = Just (prop, val)
  match _ = Nothing

indexer :: Pattern PrinterState Lisp (String, Lisp)
indexer = mkPattern' match
  where
  match (LispIndexer index@LispNumericLiteral{} val) = (,) <$> prettyPrintLisp' index <*> pure val
  match _ = mzero

indexer' :: Pattern PrinterState Lisp (String, Lisp)
indexer' = mkPattern' match
  where
  match (LispIndexer (LispVar index) val) = return (safeName (':':index), val)
  -- match (LispIndexer (LispStringLiteral index) val) = return (index, val)
  -- match (LispIndexer index val) = (,) <$> prettyPrintLisp' index <*> pure val
  match _ = mzero

lam :: Pattern PrinterState Lisp ((Maybe String, [String]), Lisp)
lam = mkPattern match
  where
  match (LispFunction name args ret) = Just ((name, args), ret)
  match _ = Nothing

app :: Pattern PrinterState Lisp (String, Lisp)
app = mkPattern' match
  where
  match (LispApp val args) = do
    lisps <- traverse prettyPrintLisp' args
    return (intercalate " " lisps, val)
  match (LispUnary Not val) = do
    lisps <- traverse prettyPrintLisp' [val]
    return (intercalate " " lisps, LispVar "not")
  match (LispUnary Negate val) = do
    lisps <- traverse prettyPrintLisp' [val]
    return (intercalate " " lisps, LispVar "-")
  match (LispUnary _ val) = do
    lisps <- traverse prettyPrintLisp' [val]
    return (intercalate " " lisps, LispVar "?")
  match _ = mzero

instanceOf :: Pattern PrinterState Lisp (Lisp, Lisp)
instanceOf = mkPattern match
  where
  match (LispInstanceOf val ty) = Just (val, ty)
  match _ = Nothing

-- unary' :: UnaryOperator -> (Lisp -> String) -> Operator PrinterState Lisp String
-- unary' op mkStr = Wrap match (++)
--   where
--   match :: Pattern PrinterState Lisp (String, Lisp)
--   match = mkPattern match'
--     where
--     match' (LispUnary op' val) | op' == op = Just (mkStr val, val)
--     match' _ = Nothing
--
-- unary :: UnaryOperator -> String -> Operator PrinterState Lisp String
-- unary op str = unary' op (const str)
--
-- negateOperator :: Operator PrinterState Lisp String
-- negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
--   where
--   isNegate (LispUnary Negate _) = True
--   isNegate _ = False

binary :: BinaryOperator -> String -> Operator PrinterState Lisp String
binary op str = AssocL match (\v1 v2 -> "(" ++ str ++ " " ++ v1 ++ " " ++ v2 ++")")
  where
  match :: Pattern PrinterState Lisp (Lisp, Lisp)
  match = mkPattern match'
    where
    match' (LispBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: [Lisp] -> StateT PrinterState Maybe String
prettyStatements sts = do
  lisps <- forM sts prettyPrintLisp'
  indentString <- currentIndent
  return $ intercalate "\n" $ map (indentString ++) lisps

-- |
-- Generate a pretty-printed string representing a Lisp expression
--
prettyPrintLisp1 :: Lisp -> String
prettyPrintLisp1 = fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintLisp'

-- |
-- Generate a pretty-printed string representing a collection of Lisp expressions at the same indentation level
--
prettyPrintLisp :: [Lisp] -> String
prettyPrintLisp = fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements

-- |
-- Generate an indented, pretty-printed string representing a Lisp expression
--
prettyPrintLisp' :: Lisp -> StateT PrinterState Maybe String
prettyPrintLisp' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: Pattern PrinterState Lisp String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable PrinterState Lisp String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "/" ++ prop ]
                  , [ Wrap indexer $ \index val -> "(nth " ++ val ++ " " ++ index ++ ")" ]
                  , [ Wrap indexer' $ \index val -> "(" ++ index ++ " " ++ val ++ ")" ]
                  , [ Wrap app $ \args val -> "(" ++ val ++ " " ++ args ++ ")" ]
                  -- , [ unary LispNew "new " ]
                  , [ Wrap lam $ \(name, args) ret -> "("
                        ++ maybe "fn" ("defn " ++) name
                        ++ " [" ++ intercalate " " args ++ "] "
                        ++ ret
                        ++ ")" ]
                  -- , [ unary     Not                  "!"
                  --   , unary     BitwiseNot           "~"
                  --   , unary     Positive             "+"
                  --   , negateOperator ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    StringAppend         "str"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">="
                    , AssocR instanceOf $ \v1 v2 -> "(= (:constructor " ++ v1 ++ ") " ++ v2 ++")" ]
                  , [ binary    EqualTo              "="
                    , binary    NotEqualTo           "/=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "and" ]
                  , [ binary    Or                   "or" ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintLisp1 th ++ " : " ++ prettyPrintLisp1 el ]
                    ]
