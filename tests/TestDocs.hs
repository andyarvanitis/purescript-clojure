{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module TestDocs where

import Prelude ()
import Prelude.Compat

import Data.Version (Version(..))

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.List ((\\))
import Data.Foldable
import System.Exit

import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as Docs
import qualified Language.PureScript.Publish as Publish

import TestUtils

publishOpts :: Publish.PublishOptions
publishOpts = Publish.defaultPublishOptions
  { Publish.publishGetVersion = return testVersion
  , Publish.publishWorkingTreeDirty = return ()
  }
  where testVersion = ("v999.0.0", Version [999,0,0] [])

main :: IO ()
main = do
  pushd "examples/docs" $ do
    Docs.Package{..} <- Publish.preparePackage publishOpts
    forM_ testCases $ \(mn, pragmas) ->
      let mdl = takeJust ("module not found in docs: " ++ mn)
                         (find ((==) mn . Docs.modName) pkgModules)
      in forM_ pragmas (flip runAssertionIO mdl)

takeJust :: String -> Maybe a -> a
takeJust msg = maybe (error msg) id

data Assertion
  -- | Assert that a particular declaration is documented with the given
  -- children
  = ShouldBeDocumented P.ModuleName String [String]
  -- | Assert that a particular declaration is not documented
  | ShouldNotBeDocumented P.ModuleName String
  -- | Assert that a particular declaration exists, but without a particular
  -- child.
  | ChildShouldNotBeDocumented P.ModuleName String String
  -- | Assert that a particular declaration has a particular type class
  -- constraint.
  | ShouldBeConstrained P.ModuleName String String
  deriving (Show)

data AssertionFailure
  -- | A declaration was not documented, but should have been
  = NotDocumented P.ModuleName String
  -- | A child declaration was not documented, but should have been
  | ChildrenNotDocumented P.ModuleName String [String]
  -- | A declaration was documented, but should not have been
  | Documented P.ModuleName String
  -- | A child declaration was documented, but should not have been
  | ChildDocumented P.ModuleName String String
  -- | A constraint was missing.
  | ConstraintMissing P.ModuleName String String
  -- | A declaration had the wrong "type" (ie, value, type, type class)
  -- Fields: declaration title, expected "type", actual "type".
  | WrongDeclarationType P.ModuleName String String String
  deriving (Show)

data AssertionResult
  = Pass
  | Fail AssertionFailure
  deriving (Show)

runAssertion :: Assertion -> Docs.Module -> AssertionResult
runAssertion assertion Docs.Module{..} =
  case assertion of
    ShouldBeDocumented mn decl children ->
      case findChildren decl (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn decl)
        Just actualChildren ->
          case children \\ actualChildren of
            [] -> Pass
            cs -> Fail (ChildrenNotDocumented mn decl cs)

    ShouldNotBeDocumented mn decl ->
      case findChildren decl (declarationsFor mn) of
        Just _ ->
          Fail (Documented mn decl)
        Nothing ->
          Pass

    ChildShouldNotBeDocumented mn decl child ->
      case findChildren decl (declarationsFor mn) of
        Just children ->
          if child `elem` children
            then Fail (ChildDocumented mn decl child)
            else Pass
        Nothing ->
          Fail (NotDocumented mn decl)

    ShouldBeConstrained mn decl tyClass ->
      case find ((==) decl . Docs.declTitle) (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn decl)
        Just Docs.Declaration{..} ->
          case declInfo of
            Docs.ValueDeclaration ty ->
              if checkConstrained ty tyClass
                then Pass
                else Fail (ConstraintMissing mn decl tyClass)
            _ ->
              Fail (WrongDeclarationType mn decl "value"
                     (Docs.declInfoToString declInfo))

  where
  declarationsFor mn =
    if P.runModuleName mn == modName
      then modDeclarations
      else fromMaybe [] (lookup mn modReExports)

  findChildren title =
    fmap childrenTitles . find ((==) title . Docs.declTitle)

  childrenTitles = map Docs.cdeclTitle . Docs.declChildren

checkConstrained :: P.Type -> String -> Bool
checkConstrained ty tyClass =
  -- Note that we don't recurse on ConstrainedType if none of the constraints
  -- match; this is by design, as constraints should be moved to the front
  -- anyway.
  case ty of
    P.ConstrainedType cs _ | any (matches tyClass) cs ->
      True
    P.ForAll _ ty' _ ->
      checkConstrained ty' tyClass
    _ ->
      False
  where
  matches className =
    (==) className . P.runProperName . P.disqualify . fst

runAssertionIO :: Assertion -> Docs.Module -> IO ()
runAssertionIO assertion mdl = do
  putStrLn ("In " ++ Docs.modName mdl ++ ": " ++ show assertion)
  case runAssertion assertion mdl of
    Pass -> pure ()
    Fail reason -> do
      putStrLn ("Failed: " <> show reason)
      exitFailure

testCases :: [(String, [Assertion])]
testCases =
  [ ("Example",
      [ -- From dependencies
        ShouldBeDocumented    (n "Prelude") "Unit" []
      , ShouldNotBeDocumented (n "Prelude") "unit"

        -- From local files
      , ShouldBeDocumented    (n "Example2") "one" []
      , ShouldNotBeDocumented (n "Example2") "two"
      ])
  , ("Example2",
      [ ShouldBeDocumented (n "Example2") "one" []
      , ShouldBeDocumented (n "Example2") "two" []
      ])

  , ("UTF8",
      [ ShouldBeDocumented (n "UTF8") "thing" []
      ])

  , ("Transitive1",
      [ ShouldBeDocumented (n "Transitive2") "transitive3" []
      ])

  , ("NotAllCtors",
      [ ShouldBeDocumented         (n "Prelude") "Boolean2" ["True"]
      , ChildShouldNotBeDocumented (n "Prelude") "Boolean2" "False"
      ])

  , ("DuplicateNames",
      [ ShouldBeDocumented    (n "Prelude")        "Unit" []
      , ShouldBeDocumented    (n "DuplicateNames") "unit" []
      , ShouldNotBeDocumented (n "Prelude")        "unit"
      ])

  , ("MultiVirtual",
      [ ShouldBeDocumented (n "MultiVirtual1") "foo" []
      , ShouldBeDocumented (n "MultiVirtual2") "bar" []
      , ShouldBeDocumented (n "MultiVirtual2") "baz" []
      ])

  , ("Clash",
      [ ShouldBeDocumented (n "Clash1") "value" []
      , ShouldBeDocumented (n "Clash1") "Type" []
      , ShouldBeDocumented (n "Clash1") "TypeClass" ["typeClassMember"]
      ])

  , ("SolitaryTypeClassMember",
      [ ShouldBeDocumented    (n "SomeTypeClass") "member" []
      , ShouldNotBeDocumented (n "SomeTypeClass") "SomeClass"
      , ShouldBeConstrained   (n "SomeTypeClass") "member" "SomeClass"
      ])

  , ("ReExportedTypeClass",
      [ ShouldBeDocumented (n "SomeTypeClass") "SomeClass" ["member"]
      ])

  , ("TypeClassWithoutMembers",
      [ ShouldBeDocumented         (n "Intermediate") "SomeClass" []
      , ChildShouldNotBeDocumented (n "Intermediate") "SomeClass" "member"
      ])

  -- Remove this after 0.9.
  , ("OldOperators",
      [ ShouldBeDocumented  (n "OldOperators2") "(>>)" []
      ])

  , ("NewOperators",
      [ ShouldBeDocumented (n "NewOperators2") "(>>>)" []
      ])
  ]

  where
  n = P.moduleNameFromString
