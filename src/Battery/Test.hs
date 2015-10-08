{-# LANGUAGE TemplateHaskell, ExistentialQuantification, StandaloneDeriving #-}

module Battery.Test where

import GHC.Stack
import Language.Haskell.Extract
import Language.Haskell.TH
import Control.Exception (throwIO, catch, Exception)
import Control.Monad (when, forM_)
import System.Console.ANSI

data Reason = forall a. Show a => NotEqual a a
deriving instance Show Reason

reasonString :: (String -> String) -> Reason -> String
reasonString f (NotEqual expected actual) = "expected " ++ f (show expected) ++ " but got " ++ f (show actual)

data AssertionFailed = AssertionFailed String Int Reason
    deriving (Show)
instance Exception AssertionFailed

assertionFailed :: String -> Int -> Reason -> IO ()
assertionFailed filename lineno reason = throwIO $ AssertionFailed filename lineno reason

type TestName = String
data TestCase = TestCase TestName (IO ())

color :: Color -> String -> String
color c s = setSGRCode [SetColor Foreground Vivid c] ++ s ++ setSGRCode []

recordFailure :: TestName -> String -> Int -> Reason -> IO ()
recordFailure name filename lineno reason = do
    putStrLn $ filename ++ "(" ++ show lineno ++ "): " ++ color Yellow name ++ " " ++ color Red "FAILED" ++ ": " ++ reasonString (color Cyan) reason
    stack <- currentCallStack
    forM_ stack $ \entry -> do
        putStrLn entry

defaultMain :: [TestCase] -> IO ()
defaultMain tests = do
    forM_ tests $ \(TestCase name action) -> do
        putStrLn $ "running " ++ name
        catch action $ \(AssertionFailed filename lineno reason) -> do
            recordFailure name filename lineno reason

testCase :: String -> IO () -> TestCase
testCase = TestCase

data Check = forall a. (Show a, Eq a) => Equal a a

verify :: Check -> Maybe Reason
verify (Equal lhs rhs) = if lhs == rhs then Nothing else Just $ NotEqual lhs rhs

assert' :: String -> Int -> Check -> IO ()
assert' filename lineno check = do
    case verify check of
        Just reason -> assertionFailed filename lineno reason
        Nothing -> return ()

assert :: Q Exp
assert = do
    loc <- location
    let filename = loc_filename loc
    let (lineno, _) = loc_start loc
    [| assert' filename lineno |]

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
