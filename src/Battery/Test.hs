{-# LANGUAGE TemplateHaskell #-}

module Battery.Test where

import Language.Haskell.Extract
import Language.Haskell.TH
import Control.Exception (throwIO, catch, Exception)
import Control.Monad (when, forM_)

type Reason = String
data AssertionFailed = AssertionFailed Reason
    deriving (Show)

instance Exception AssertionFailed

assertionFailed :: Reason -> IO ()
assertionFailed = throwIO . AssertionFailed

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual expected actual = do
    when (expected /= actual) $ do
        assertionFailed $ "expected " ++ show expected ++ " did not equal actual " ++ show actual

type TestName = String
data TestCase = TestCase TestName (IO ())

recordFailure :: TestName -> Reason -> IO ()
recordFailure name reason = do
    putStrLn $ name ++ " failed: " ++ reason

defaultMain :: [TestCase] -> IO ()
defaultMain tests = do
    forM_ tests $ \(TestCase name action) -> do
        putStrLn $ "running " ++ name
        catch action $ \(AssertionFailed reason) -> do
            putStrLn $ name ++ " failed: " ++ reason

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
