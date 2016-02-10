{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module generates code in the simplified Lisp intermediate representation from Purescript code
--
module Language.PureScript.CodeGen.Lisp
  ( module AST
  , module Common
  , moduleToLisp
  ) where

import Prelude ()
import Prelude.Compat

import Data.List ((\\), delete, intersect, sort, sortBy)
import Data.Function (on)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Map as M
import qualified Data.Traversable as T

import Control.Arrow ((&&&))
import Control.Monad (replicateM, forM)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.Crash
import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.Lisp.AST as AST
import Language.PureScript.CodeGen.Lisp.Common as Common
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.CodeGen.Lisp.Optimizer
import Language.PureScript.Options
import Language.PureScript.Sugar.TypeClasses (superClassDictionaryNames)
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Environment as E
import qualified Language.PureScript.Types as T

import System.FilePath.Posix ((</>))

-- |
-- Generate code in the simplified Lisp intermediate representation for all declarations in a
-- module.
--
moduleToLisp
  :: forall m
   . (Applicative m, Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => E.Environment
  -> Module Ann
  -> Maybe Lisp
  -> m [Lisp]
moduleToLisp env (Module coms mn imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    lispImports <- T.traverse importToLisp . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ imps
    lispDecls <- mapM bindToLisp decls
    optimized <- T.traverse (T.traverse optimize) lispDecls
    comments <- not <$> asks optionsNoComments
    let genclass = if isMain mn then [LispApp (LispVar ":gen-class") []] else []
    let namespace = LispApp (LispVar "ns") $ [LispVar $ runModuleName mn ++ ".core"] ++ genclass
    let header = if comments && not (null coms) then LispComment coms namespace else namespace
    let foreign' = if not (null foreigns)
                     then [LispApp (LispVar "load") [LispStringLiteral "foreign"]]
                     else []
    let moduleBody = header : foreign' ++ lispImports ++ declarations ++ concat optimized
    return $ moduleBody
  where

  -- |
  -- Extracts all declaration names from a binding group.
  --
  getNames :: Bind Ann -> [Ident]
  getNames (NonRec ident _) = [ident]
  getNames (Rec vals) = map fst vals

  -- |
  -- Generates Lisp code for a module import, binding the required module
  -- to the alternative
  --
  -- importToLisp :: M.Map ModuleName ModuleName -> ModuleName -> m Lisp
  importToLisp :: ModuleName -> m Lisp
  importToLisp mn' = do
    path <- asks optionsRequirePath
    -- let mnSafe = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
    let moduleBody = maybe id (</>) path $ runModuleName mn'
    return $ LispApp (LispVar "require") [LispVar ('\'' : '[' : moduleBody ++ ".core :as " ++ moduleBody ++ "]")]

  -- |
  -- Forward declarations of values
  --
  declarations :: [Lisp]
  declarations = LispApp (LispVar "declare") . replicate 1 . LispVar . identToLisp . snd . fst <$>
                   (filter inModule . M.toList $ E.names env)
    where
    inModule :: ((ModuleName, a), (b, E.NameKind, c)) -> Bool
    inModule ((mn', _), (_, kind, _)) = mn' == mn && kind /= E.External

  -- |
  -- Generate code in the simplified Lisp intermediate representation for a declaration
  --
  bindToLisp :: Bind Ann -> m [Lisp]
  bindToLisp (NonRec ident val) = return <$> nonRecToLisp ident val
  bindToLisp (Rec vals) = forM vals (uncurry nonRecToLisp)

  -- |
  -- Generate code in the simplified Lisp intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  --
  nonRecToLisp :: Ident -> Expr Ann -> m Lisp
  nonRecToLisp i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    -- withoutComment <- asks optionsNoComments
    -- if withoutComment
    --    then nonRecToLisp i (modifyAnn removeComments e)
    --    else LispComment com <$> nonRecToLisp i (modifyAnn removeComments e)
    LispComment com <$> nonRecToLisp i (modifyAnn removeComments e)
  nonRecToLisp (Ident "main") val| isMain mn = do
    lisp <- valueToLisp val
    return $ LispFunction (Just "-main") ["& args"] (LispBlock [LispReturn (LispApp lisp [])])
  nonRecToLisp ident val = do
    lisp <- valueToLisp val
    return $ LispVariableIntroduction (identToLisp ident) (Just lisp)

  -- |
  -- Generate code in the simplified Lisp intermediate representation for a variable based on a
  -- PureScript identifier.
  --
  var :: Ident -> Lisp
  var = LispVar . identToLisp

  -- |
  -- Generate code in the simplified Lisp intermediate representation for a value or expression.
  --
  valueToLisp :: Expr Ann -> m Lisp
  valueToLisp (Literal (pos, _, _, _) l) =
    maybe id rethrowWithPosition pos $ literalToValueLisp l
  valueToLisp (Var (_, _, _, Just (IsConstructor _ [])) name) =
    return $ LispApp (qualifiedToLisp id name) []
  valueToLisp (Var (_, _, _, Just (IsConstructor _ _)) name) =
    return $ qualifiedToLisp id name
  valueToLisp (Accessor _ prop val) =
    LispIndexer (LispVar prop) <$> valueToLisp val
  -- TODO: use a more efficient way of copying/updating the hashmap?
  valueToLisp (ObjectUpdate (_, _, Just ty, _) obj ps) = do
    obj' <- valueToLisp obj
    updatedFields <- mapM (sndM valueToLisp) ps
    let origKeys = (allKeys ty) \\ (fst <$> updatedFields)
        origFields = (\key -> (key, LispIndexer (LispVar key) obj')) <$> origKeys
    return $ LispObjectLiteral . sortBy (compare `on` fst) $ origFields ++ updatedFields
    where
    allKeys :: T.Type -> [String]
    allKeys (T.TypeApp (T.TypeConstructor _) r@(T.RCons {})) = fst <$> (fst $ T.rowToList r)
    allKeys (T.ForAll _ t _) = allKeys t
    allKeys _ = error $ show "Not a recognized row type"
  valueToLisp (Abs _ arg val) = do
    ret <- valueToLisp val
    return $ LispFunction Nothing [identToLisp arg] ret
  valueToLisp e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToLisp args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just IsTypeClassConstructor) (Qualified mn' (Ident classname)) ->
        let Just (_, constraints, fns) = findClass (Qualified mn' (ProperName classname)) in
        return . LispObjectLiteral $ zip ((sort $ superClassDictionaryNames constraints) ++ (fst <$> fns))
                                         args'
      _ -> flip (foldl (\fn a -> LispApp fn [a])) args' <$> valueToLisp f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToLisp (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
    return $ if mn' == mn
             then LispVar $ identToLisp ident
             else varToLisp qi
  valueToLisp (Var (_, _, _, Just IsForeign) ident) =
    error $ "Encountered an unqualified reference to a foreign ident " ++ showQualified showIdent ident
  valueToLisp (Var _ ident) =
    return $ varToLisp ident
  valueToLisp (Case (maybeSpan, _, _, _) values binders) = do
    vals <- mapM valueToLisp values
    bindersToLisp maybeSpan binders vals
  valueToLisp (Let _ ds val) = do
    ds' <- concat <$> mapM bindToLisp ds
    ret <- valueToLisp val
    return $ if any isFunction ds'
               then let varNames = concatMap varName ds'
                    in LispApp (LispVar "letfn") $
                         LispArrayLiteral (concatMap (fnval varNames) ds') : [LispReturn $ replaceVars varNames ret]
               else LispApp (LispVar "let") $ LispArrayLiteral (concatMap varval ds') : [LispReturn ret]
    where
    isFunction :: Lisp -> Bool
    isFunction (LispVariableIntroduction _ (Just LispFunction{})) = True
    isFunction (LispFunction{}) = True
    isFunction _ = False

    replaceVars :: [Lisp] -> Lisp -> Lisp
    replaceVars ls = everywhereOnLisp (\l -> if l `elem` ls then LispApp l [] else l)

    varName :: Lisp -> [Lisp]
    varName (LispVariableIntroduction name (Just (LispFunction{}))) = []
    varName (LispVariableIntroduction name (Just _)) = [LispVar name]
    varName _ = []

    fnval :: [Lisp] -> Lisp -> [Lisp]
    fnval vs (LispVariableIntroduction name (Just (LispFunction _ args body))) =
      [LispApp (LispVar name) [LispArrayLiteral (LispVar <$> args), replaceVars vs body]]
    fnval vs (LispFunction (Just name) args body) =
      [LispApp (LispVar name) [LispArrayLiteral (LispVar <$> args), replaceVars vs body]]
    fnval _ (LispVariableIntroduction var' (Just val')) =
      [LispApp (LispVar var') [LispArrayLiteral [], val']]
    fnval _ _ = []

    varval :: Lisp -> [Lisp]
    varval (LispVariableIntroduction var' (Just val')) = [LispVar var', val']
    varval _ = []
  valueToLisp (Constructor _ _ (ProperName ctor) fields) =
    return $ LispFunction Nothing
                          (fields')
                          (LispReturn $ LispObjectLiteral (("constructor", LispVar (safeName (':':ctor))) :
                                                           zip fields' (LispVar <$> fields')))
    where
    fields' = identToLisp <$> fields
  literalToValueLisp :: Literal (Expr Ann) -> m Lisp
  literalToValueLisp (NumericLiteral (Left i)) = return $ LispNumericLiteral (Left i)
  literalToValueLisp (NumericLiteral (Right n)) = return $ LispNumericLiteral (Right n)
  literalToValueLisp (StringLiteral s) = return $ LispStringLiteral s
  literalToValueLisp (CharLiteral c) = return $ LispCharLiteral c
  literalToValueLisp (BooleanLiteral b) = return $ LispBooleanLiteral b
  literalToValueLisp (ArrayLiteral xs) = LispArrayLiteral <$> mapM valueToLisp xs
  literalToValueLisp (ObjectLiteral ps) = LispObjectLiteral <$> mapM (sndM valueToLisp) ps

  -- |
  -- Generate code in the simplified Lisp intermediate representation for a reference to a
  -- variable.
  --
  varToLisp :: Qualified Ident -> Lisp
  varToLisp (Qualified Nothing ident) = var ident
  varToLisp qual = qualifiedToLisp id qual

  -- |
  -- Generate code in the simplified Lisp intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToLisp :: (a -> Ident) -> Qualified a -> Lisp
  qualifiedToLisp f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = LispVar . runIdent $ f a
  -- qualifiedToLisp f (Qualified (Just mn') a) | mn /= mn' = accessor (f a) (LispVar (moduleNameToLisp mn'))
  qualifiedToLisp f (Qualified (Just mn') a)
    | mn /= mn' = LispAccessor (identToLisp $ f a) (LispVar (runModuleName mn'))
  qualifiedToLisp f (Qualified _ a) = LispVar $ identToLisp (f a)

  -- |
  -- Generate code in the simplified Lisp intermediate representation for pattern match binders
  -- and guards.
  --
  bindersToLisp :: Maybe SourceSpan -> [CaseAlternative Ann] -> [Lisp] -> m Lisp
  bindersToLisp maybeSpan binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith LispVariableIntroduction valNames (map Just vals)
    lisps <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToLisp result
      go valNames ret bs
    return $ LispBlock (assignments ++ concat lisps ++ [LispThrow $ failedPatternError valNames])
    where
      go :: [String] -> [Lisp] -> [Binder Ann] -> m [Lisp]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToLisp v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToLisp"

      failedPatternError :: [String] -> Lisp
      failedPatternError names = LispApp (LispVar "str") (LispStringLiteral failedPatternMessage : zipWith valueError names vals)
      failedPatternMessage :: String
      failedPatternMessage = "Failed pattern match" ++ maybe "" (((" at " ++ runModuleName mn ++ " ") ++) . displayStartEndPos) maybeSpan ++ ": "

      valueError :: String -> Lisp -> Lisp
      valueError _ l@(LispNumericLiteral _) = l
      valueError _ l@(LispStringLiteral _)  = l
      valueError _ l@(LispBooleanLiteral _) = l
      valueError s _                        = LispApp (LispVar "pr-str") [LispVar s]

      guardsToLisp :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [Lisp]
      guardsToLisp (Left gs) = forM gs $ \(cond, val) -> do
        cond' <- valueToLisp cond
        done  <- valueToLisp val
        return $ LispIfElse cond' (LispReturn done) Nothing
      guardsToLisp (Right v) = return . LispReturn <$> valueToLisp v

  -- |
  -- Generate code in the simplified Lisp intermediate representation for a pattern match
  -- binder.
  --
  binderToLisp :: String -> [Lisp] -> Binder Ann -> m [Lisp]
  binderToLisp _ done (NullBinder{}) = return done
  binderToLisp varName done (LiteralBinder _ l) =
    literalToBinderLisp varName done l
  binderToLisp varName done (VarBinder _ ident) =
    return (LispVariableIntroduction (identToLisp ident) (Just (LispVar varName)) : done)
  binderToLisp varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToLisp varName done b
  binderToLisp varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ (Qualified _ (ProperName ctor)) bs) = do
    lisps <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> lisps
      SumType ->
        [LispIfElse (LispInstanceOf (LispVar varName) (LispVar (safeName (':':ctor))))
                  (LispBlock lisps)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [Lisp] -> m [Lisp]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      lisp <- binderToLisp argVar done'' binder
      return (LispVariableIntroduction argVar (Just (LispIndexer (LispVar $ runIdent field) (LispVar varName))) : lisp)
  binderToLisp _ _ ConstructorBinder{} =
    internalError "binderToLisp: Invalid ConstructorBinder in binderToLisp"
  binderToLisp varName done (NamedBinder _ ident binder) = do
    lisp <- binderToLisp varName done binder
    return (LispVariableIntroduction (identToLisp ident) (Just (LispVar varName)) : lisp)

  literalToBinderLisp :: String -> [Lisp] -> Literal (Binder Ann) -> m [Lisp]
  literalToBinderLisp varName done (NumericLiteral num) =
    return [LispIfElse (LispBinary EqualTo (LispVar varName) (LispNumericLiteral num)) (LispBlock done) Nothing]
  literalToBinderLisp varName done (CharLiteral c) =
    return [LispIfElse (LispBinary EqualTo (LispVar varName) (LispCharLiteral c)) (LispBlock done) Nothing]
  literalToBinderLisp varName done (StringLiteral str) =
    return [LispIfElse (LispBinary EqualTo (LispVar varName) (LispStringLiteral str)) (LispBlock done) Nothing]
  literalToBinderLisp varName done (BooleanLiteral True) =
    return [LispIfElse (LispVar varName) (LispBlock done) Nothing]
  literalToBinderLisp varName done (BooleanLiteral False) =
    return [LispIfElse (LispUnary Not (LispVar varName)) (LispBlock done) Nothing]
  literalToBinderLisp varName done (ObjectLiteral bs) = go done bs
    where
    go :: [Lisp] -> [(String, Binder Ann)] -> m [Lisp]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      lisp <- binderToLisp propVar done'' binder
      return (LispVariableIntroduction propVar (Just (LispIndexer (LispVar prop) (LispVar varName))) : lisp)
  literalToBinderLisp varName done (ArrayLiteral bs) = do
    lisp <- go done 0 bs
    return [LispIfElse (LispBinary EqualTo (LispApp (LispVar "count")
                       [LispVar varName]) (LispNumericLiteral (Left (fromIntegral $ length bs)))) (LispBlock lisp) Nothing]
    where
    go :: [Lisp] -> Integer -> [Binder Ann] -> m [Lisp]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      lisp <- binderToLisp elVar done'' binder
      return (LispVariableIntroduction elVar (Just (LispIndexer (LispNumericLiteral (Left index)) (LispVar varName))) : lisp)

  -- |
  -- Find a class in scope by name, retrieving its list of constraints, function names and types.
  --
  findClass :: Qualified (ProperName ClassName) -> Maybe ([String], [T.Constraint], [(String, T.Type)])
  findClass name
    | Just (params, fns, constraints) <- M.lookup name (E.typeClasses env),
      fns' <- (\(i,t) -> (runIdent i, t)) <$> fns
      = Just (fst <$> params, constraints, (sortBy (compare `on` normalizedName . fst) fns'))
  findClass _ = Nothing

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False
