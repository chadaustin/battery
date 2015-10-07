{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

module Battery.Test where

import Language.Haskell.Extract
import Language.Haskell.TH
import Control.Exception (throwIO, catch, Exception)
import Control.Monad (when, forM_)
import System.Console.ANSI

data Reason = forall a. Show a => NotEqual a a

instance Show Reason where
    show (NotEqual expected actual) = "NotEqual " ++ show expected ++ " " ++ show actual

reasonString :: (String -> String) -> Reason -> String
reasonString f (NotEqual expected actual) = "expected " ++ f (show expected) ++ " but got " ++ f (show actual)

data AssertionFailed = AssertionFailed Reason
    deriving (Show)

instance Exception AssertionFailed

assertionFailed :: Reason -> IO ()
assertionFailed = throwIO . AssertionFailed

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual expected actual = do
    when (expected /= actual) $ do
        assertionFailed $ NotEqual expected actual

type TestName = String
data TestCase = TestCase TestName (IO ())

yellow :: String -> String
yellow s = setSGRCode [SetColor Foreground Vivid Yellow] ++ s ++ setSGRCode []

red :: String -> String
red s = setSGRCode [SetColor Foreground Vivid Red] ++ s ++ setSGRCode []

green :: String -> String
green s = setSGRCode [SetColor Foreground Vivid Green] ++ s ++ setSGRCode []

recordFailure :: TestName -> Reason -> IO ()
recordFailure name reason = do
    putStrLn $ yellow name ++ " " ++ red "FAILED" ++ ": " ++ reasonString green reason

defaultMain :: [TestCase] -> IO ()
defaultMain tests = do
    forM_ tests $ \(TestCase name action) -> do
        putStrLn $ "running " ++ name
        catch action $ \(AssertionFailed reason) -> do
            recordFailure name reason

testCase :: String -> IO () -> TestCase
testCase = TestCase

testMain :: Q [Dec]
testMain = do
    body <- [| defaultMain $(collectTests) |]
    return [FunD (mkName "main")
        [ Clause [] (NormalB body) []
        ]]

listGenerator :: String -> ExpQ
listGenerator beginning = functionExtractorMap beginning (return $ VarE $ mkName "testCase")

collectTests :: ExpQ
collectTests = listGenerator "^test_"
