-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.Lisp.Common where

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Crash
import Language.PureScript.Names

-- |
-- Convert an Ident into a valid Lisp identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved Lisp identifiers are prefixed with '!!'.
--
--  * Symbols are prefixed with '!' followed by a symbol name or their ordinal value.
--
identToLisp :: Ident -> String
identToLisp (Ident name) = safeName name
identToLisp (Op op) = concatMap identCharToString op
identToLisp (GenIdent _ _) = internalError "GenIdent in identToLisp"

safeName :: String -> String
safeName name
  | nameIsLispReserved name || nameIsLispBuiltIn name = "!!" ++ name
  | (':':name') <- name = ':' : concatMap identCharToString name'
  | (c:_) <- name, isLower c = '!' : concatMap identCharToString name
  | otherwise = concatMap identCharToString name

-- |
-- Test if a string is a valid Lisp identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToLisp (Ident s)

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
identCharToString '.' = "!dot"
identCharToString '$' = "!dollar"
identCharToString '~' = "!tilde"
identCharToString '=' = "!eq"
identCharToString '<' = "!less"
identCharToString '>' = "!greater"
identCharToString '!' = "!"
identCharToString '#' = "!hash"
identCharToString '%' = "!percent"
identCharToString '^' = "!up"
identCharToString '&' = "!amp"
identCharToString '|' = "!bar"
identCharToString '*' = "!times"
identCharToString '/' = "!div"
identCharToString '+' = "!plus"
identCharToString '-' = "!minus"
identCharToString ':' = "!colon"
identCharToString '\\' = "!bslash"
identCharToString '?' = "!qmark"
identCharToString '@' = "!at"
identCharToString '\'' = "*"
identCharToString c = '!' : show (ord c)

-- |
-- Checks whether an identifier name is reserved in Lisp.
--
nameIsLispReserved :: String -> Bool
nameIsLispReserved name =
  name `elem` lispAnyReserved

-- |
-- Checks whether a name matches a built-in value in Lisp.
--
nameIsLispBuiltIn :: String -> Bool
nameIsLispBuiltIn name =
  elem name
    [
    ]

lispAnyReserved :: [String]
lispAnyReserved =
  concat
    [ javaReserved
    , lispLiterals
    ]

javaReserved :: [String]
javaReserved =
  [ "Boolean"
  , "Byte"
  , "Character"
  , "Integer"
  , "Double"
  , "Enum"
  , "Float"
  , "Long"
  , "Short"
  , "String"
  , "Void"
  ]

lispLiterals :: [String]
lispLiterals = []

normalizedName :: String -> String
normalizedName ('_' : s) | last s == '_', s' <- init s, nameIsLispReserved s' = s'
normalizedName s = s
