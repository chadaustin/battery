{-# LANGUAGE TemplateHaskell, ExistentialQuantification, StandaloneDeriving, TypeFamilies, FlexibleContexts #-}

module Battery.Test where

import GHC.Stack
import Language.Haskell.TH
import Control.Exception (throwIO, catch, Exception)
import Control.Monad (when, forM_)
import System.Console.ANSI
import Data.List
import Data.Maybe
import Text.Regex.Posix

data AssertionFailed = forall a. (Show a, FailureReason a) => AssertionFailed String Int a
instance Exception AssertionFailed
deriving instance Show AssertionFailed

assertionFailed :: (FailureReason a, Show a) => String -> Int -> a -> IO ()
assertionFailed filename lineno reason = throwIO $ AssertionFailed filename lineno reason

type TestName = String
data TestCase = TestCase TestName (IO ())

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

recordTestFailure :: FailureReason a => TestName -> String -> Int -> a -> IO ()
recordTestFailure name filename lineno reason = do
    putStrLn $ color Vivid Red "FAILED"
    putStrLn $ color Vivid Yellow (filename ++ "(" ++ show lineno ++ ")") ++ ": " ++ formatReason (color Vivid Cyan) reason
    stack <- currentCallStack
    forM_ stack $ \entry -> do
        putStrLn entry

defaultMain :: [TestCase] -> IO ()
defaultMain tests = do
    forM_ tests $ \(TestCase name action) -> do
        recordTestStart name
        case name of
            'x':_ -> recordTestDisabled name
            _ -> do
                catch (action >> recordTestSuccess name) $ \(AssertionFailed filename lineno reason) -> do
                    recordTestFailure name filename lineno reason

testCase :: String -> IO () -> TestCase
testCase = TestCase

class FailureReason a where
    formatReason :: (String -> String) -> a -> String

class Check a where
    type Failure a
    check :: a -> Maybe (Failure a)

data Equal a = (Show a, Eq a) => Equal a a
data NotEqualReason a = Show a => NotEqualReason a a
deriving instance Show (NotEqualReason a)

instance FailureReason (NotEqualReason a) where
    formatReason f (NotEqualReason expected actual) = "expected " ++ f (show expected) ++ " but got " ++ f (show actual)
instance Check (Equal a) where
    type Failure (Equal a) = NotEqualReason a
    check (Equal expected actual) = if expected == actual then Nothing else Just $ NotEqualReason expected actual

assert' :: (Check a, FailureReason (Failure a), Show (Failure a)) => String -> Int -> a -> IO ()
assert' filename lineno chk = do
    case check chk of
        Just reason -> assertionFailed filename lineno reason
        Nothing -> return ()

assert :: Q Exp
assert = do
    loc <- location
    let filename = loc_filename loc
    let (lineno, _) = loc_start loc
    [| assert' filename lineno |]

assertEqual' :: (Show a, Eq a) => String -> Int -> a -> a -> IO ()
assertEqual' filename lineno expected actual = do
    assert' filename lineno $ Equal expected actual

assertEqual :: Q Exp
assertEqual = do
    loc <- location
    let filename = loc_filename loc
    let (lineno, _) = loc_start loc
    [| assertEqual' filename lineno |]

extractAllFunctions :: String -> Q [String]
extractAllFunctions pattern = do
    loc <- location
    file <- runIO $ readFile $ loc_filename loc
    return $ nub $ filter (=~pattern) $ map fst $ concat $ map lex $ lines file

functionExtractorMap :: String -> ExpQ -> ExpQ
functionExtractorMap pattern funcName = do
    functions <- extractAllFunctions pattern
    fn <- funcName
    let makePair n = do
            nm' <- lookupValueName n
            return $ case nm' of
                Just nm -> Just $ AppE (AppE (fn) (LitE $ StringL $ n)) (VarE nm)
                Nothing -> Nothing
    fmap (ListE . catMaybes) $ mapM makePair functions

testMain :: Q [Dec]
testMain = do
    let tests = collectTests
    body <- [| defaultMain $(tests) |]
    return [FunD (mkName "main")
        [ Clause [] (NormalB body) []
        ]]

listGenerator :: String -> ExpQ
listGenerator beginning = functionExtractorMap beginning (return $ VarE $ mkName "testCase")

collectTests :: ExpQ
collectTests = listGenerator "^x?test_"
