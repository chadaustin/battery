{-# LANGUAGE TemplateHaskell, ExistentialQuantification, StandaloneDeriving, TypeFamilies, FlexibleContexts, RecordWildCards #-}

module Battery.Test where

import GHC.Stack
import Language.Haskell.TH
import Control.Exception (throwIO, catch, Exception)
import Control.Monad
import System.Console.ANSI
import Data.List
import Data.Maybe
import Text.Regex.Posix
import Test.QuickCheck

type AssertLocation = (String, Int)

toAssertLocation :: Loc -> AssertLocation
toAssertLocation Loc{..} = (loc_filename, fst loc_start)

data AssertionFailed = AssertionFailed AssertLocation FailureReason
instance Exception AssertionFailed

instance Show AssertionFailed where
    show (AssertionFailed loc reason) = "AssertionFailed " ++ show loc ++ " " ++ reason id

assertionFailed :: AssertLocation -> FailureReason -> IO ()
assertionFailed loc reason = throwIO $ AssertionFailed loc reason

type TestName = String
data Test = TestCase TestName (IO ()) | forall a. Testable a => TestProp TestName a

color :: ColorIntensity -> Color -> String -> String
color i c s = setSGRCode [SetColor Foreground i c] ++ s ++ setSGRCode []

recordTestStart :: TestName -> IO ()
recordTestStart name = do
    putStr $ name ++ ": "

recordTestSuccess :: TestName -> IO ()
recordTestSuccess _ = do
    putStrLn $ color Vivid Green "PASS"

recordTestDisabled :: TestName -> IO ()
recordTestDisabled _ = do
    putStrLn $ color Dull White "DISABLED"

recordTestFailure :: TestName -> AssertLocation -> FailureReason -> IO ()
recordTestFailure name (filename, lineno) reason = do
    putStrLn $ color Vivid Red "FAILED"
    putStrLn $ color Vivid Yellow (filename ++ "(" ++ show lineno ++ ")") ++ ": " ++ reason (color Vivid Cyan)
    stack <- currentCallStack
    forM_ stack $ \entry -> do
        putStrLn entry

testName :: Test -> TestName
testName (TestCase name _) = name
testName (TestProp name _) = name

defaultMain :: [Test] -> IO ()
defaultMain tests = do
    forM_ tests $ \test -> do
        let name = testName test
        recordTestStart $ name
        case name of
            'x':_ -> recordTestDisabled name
            _ -> case test of
                (TestCase _ action) -> do
                    catch (action >> recordTestSuccess name) $ \(AssertionFailed loc reason) -> do
                        recordTestFailure name loc reason
                (TestProp _ testable) -> do
                    result <- quickCheckWithResult stdArgs{chatty=False} testable
                    case result of
                        Success{..} -> recordTestSuccess name
                        _ -> recordTestFailure name ("unknown", 0) $ \_ -> show result

testCase :: String -> IO () -> Test
testCase = TestCase

testProp :: Testable a => TestName -> a -> Test
testProp = TestProp

-- TODO: actually enable quickcheck support
--testProp :: Testable a => String -> a -> TestCase
--testProp = TestProp

type FailureReason = (String -> String) -> String

class Check a where
    check :: a -> Maybe FailureReason

data Equal a = (Show a, Eq a) => Equal a a
instance Check (Equal a) where
    check (Equal expected actual) =
        if expected == actual then
            Nothing
        else
            Just $ \f -> "expected " ++ f (show expected) ++ " but got " ++ f (show actual)

assert' :: Check a => AssertLocation -> a -> IO ()
assert' loc chk = do
    case check chk of
        Just reason -> assertionFailed loc reason
        Nothing -> return ()

assert :: Q Exp
assert = do
    loc <- fmap toAssertLocation location
    [| assert' loc |]

multiApp :: Exp -> [Exp] -> Exp
multiApp fn [] = error "Cannot apply zero arguments"
multiApp fn [param] = AppE fn param
multiApp fn params = AppE (multiApp fn $ init params) (last params)

makeAssert :: Int -> Name -> Q Exp
makeAssert arity checkConstructor = do
    names <- replicateM arity $ newName "x"
    a <- assert
    return $ LamE (fmap VarP names) $ AppE a $ multiApp (ConE checkConstructor) $ fmap VarE names

assertEqual :: Q Exp
assertEqual = makeAssert 2 'Equal

-- TODO: import Data.Safe
headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay [] = Nothing

functionExtractorMap :: [(String, Exp)] -> Q Exp
functionExtractorMap patterns = do
    loc <- location
    file <- runIO $ readFile $ loc_filename loc

    let syms = nub $ map fst $ concat $ map lex $ lines file
    pairs <- forM syms $ \sym -> do
        maybeSymName <- lookupValueName sym
        case maybeSymName of
            Just symName -> do
                fmap (headMay . catMaybes) $ forM patterns $ \(pattern, fn) -> do
                    if sym =~ pattern then
                        return $ Just $ AppE (AppE fn (LitE $ StringL sym)) (VarE symName)
                    else
                        return Nothing
            Nothing -> return Nothing

    return $ ListE $ catMaybes $ (pairs :: [Maybe Exp])

testMain :: Q [Dec]
testMain = do
    let tests = collectTests
    body <- [| defaultMain $(tests) |]
    return [FunD (mkName "main")
        [ Clause [] (NormalB body) []
        ]]

collectTests :: Q Exp
collectTests = functionExtractorMap
    [ ("^x?test_", VarE 'testCase)
    , ("^x?prop_", VarE 'testProp)
    ]
