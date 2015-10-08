{-# LANGUAGE TemplateHaskell, ExistentialQuantification, StandaloneDeriving #-}

module Battery.Test where

import GHC.Stack
import Language.Haskell.TH
import Control.Exception (throwIO, catch, Exception)
import Control.Monad (when, forM_)
import System.Console.ANSI
import Data.List
import Data.Maybe
import Text.Regex.Posix

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

recordTestStart :: TestName -> IO ()
recordTestStart name = do
    putStr $ name ++ ": "

recordTestSuccess :: TestName -> IO ()
recordTestSuccess _ = do
    putStrLn $ color Green "PASS"

recordTestFailure :: TestName -> String -> Int -> Reason -> IO ()
recordTestFailure name filename lineno reason = do
    putStrLn $ color Red "FAILED"
    putStrLn $ color Yellow (filename ++ "(" ++ show lineno ++ ")") ++ ": " ++ reasonString (color Cyan) reason
    stack <- currentCallStack
    forM_ stack $ \entry -> do
        putStrLn entry

defaultMain :: [TestCase] -> IO ()
defaultMain tests = do
    forM_ tests $ \(TestCase name action) -> do
        recordTestStart name
        catch (action >> recordTestSuccess name) $ \(AssertionFailed filename lineno reason) -> do
            recordTestFailure name filename lineno reason

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
collectTests = listGenerator "^test_"
