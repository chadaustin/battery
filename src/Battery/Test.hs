{-# LANGUAGE TemplateHaskell, ExistentialQuantification, StandaloneDeriving, TypeFamilies, FlexibleContexts, RecordWildCards #-}

module Battery.Test where

import Safe
import GHC.Stack
import Language.Haskell.TH
import Control.Exception (throwIO, catch, Exception)
import Control.Monad
import System.Console.ANSI
import Data.List
import Data.Maybe
import Text.Regex.Posix
import Test.QuickCheck
import System.IO
import Data.IORef
import System.FilePath
import System.Directory

type TestName = String
type Location = (FilePath, Int)
type FailureReason = (String -> String) -> String

data TestResult = TRSuccess | TRFailure Location FailureReason | TRDisabled
data Test = Test TestName Location (IO TestResult)

toLocation :: Loc -> Location
toLocation Loc{..} = (loc_filename, fst loc_start)

data AssertionFailed = AssertionFailed Location FailureReason
instance Exception AssertionFailed

instance Show AssertionFailed where
    show (AssertionFailed loc reason) = "AssertionFailed " ++ show loc ++ " " ++ reason id

assertionFailed :: Location -> FailureReason -> IO ()
assertionFailed loc reason = throwIO $ AssertionFailed loc reason

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

recordTestFailure :: TestName -> Location -> FailureReason -> IO ()
recordTestFailure name (filename, lineno) reason = do
    putStrLn $ color Vivid Red "FAILED"
    putStrLn $ color Vivid Yellow (filename ++ "(" ++ show lineno ++ ")") ++ ": " ++ reason (color Vivid Cyan)
    stack <- currentCallStack
    forM_ stack $ \entry -> do
        putStrLn entry

defaultMain :: [Test] -> IO ()
defaultMain tests = do
    forM_ tests $ \(Test name testLocation action) -> do
        recordTestStart $ name
        result <- action
        case result of
            TRSuccess -> recordTestSuccess name
            TRFailure failureLocation reason -> recordTestFailure name failureLocation reason
            TRDisabled -> recordTestDisabled name

testCase :: TestName -> Location -> IO () -> Test
testCase name location action = Test name location $ do
    catch (action >> return TRSuccess) $ \(AssertionFailed loc reason) -> do
        return $ TRFailure loc reason

testProperty :: Testable a => TestName -> Location -> a -> Test
testProperty name location testable = Test name location $ do
    result <- quickCheckWithResult stdArgs{chatty=False} testable
    case result of
        Success{..} -> return TRSuccess
        _ -> return $ TRFailure location $ \_ -> show result

testDisabled :: TestName -> Location -> a -> Test
testDisabled name location _ = Test name location $ return TRDisabled

class Check a where
    check :: a -> Maybe FailureReason

data Equal a = (Show a, Eq a) => Equal a a
instance Check (Equal a) where
    check (Equal expected actual) =
        if expected == actual then
            Nothing
        else
            Just $ \f -> "expected " ++ f (show expected) ++ " but got " ++ f (show actual)

assert' :: Check a => Location -> a -> IO ()
assert' loc chk = do
    case check chk of
        Just reason -> assertionFailed loc reason
        Nothing -> return ()

assert :: Q Exp
assert = do
    loc <- fmap toLocation location
    [| assert' loc |]

multiApp :: Exp -> [Exp] -> Exp
multiApp = foldl AppE

makeAssert :: Int -> Name -> Q Exp
makeAssert arity checkConstructor = do
    names <- replicateM arity $ newName "x"
    a <- assert
    return $ LamE (fmap VarP names) $ AppE a $ multiApp (ConE checkConstructor) $ fmap VarE names

assertEqual :: Q Exp
assertEqual = makeAssert 2 'Equal

functionExtractorMap :: [(String, Exp)] -> Q Exp
functionExtractorMap patterns = do
    loc <- location
    file <- runIO $ readFile $ loc_filename loc

    let extractToken :: (Int, String) -> Maybe (Int, String)
        extractToken (lineno, line) = case lex line of
            [] -> Nothing
            [(eme, _)] -> Just (lineno, eme)

    -- reverse twice because the last duplicate symbol is usually the definition
    -- WISH: it would be so nice if we could just ask TH for the line number of a definition...
    let syms = reverse $ nubBy (\a b -> snd a == snd b) $ reverse $ catMaybes $ map extractToken $ zip [1..] $ lines file
    pairs <- forM syms $ \(lineno, sym) -> do
        maybeSymName <- lookupValueName sym
        case maybeSymName of
            Just symName -> do
                fmap (headMay . catMaybes) $ forM patterns $ \(pattern, fn) -> do
                    if sym =~ pattern then do
                        let tloc = (loc_filename loc, lineno)
                        tl <- [| tloc |]
                        return $ Just $ AppE (AppE (AppE fn (LitE $ StringL sym)) tl) (VarE symName)
                    else do
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

-- Python's os.walk for Haskell
pathWalk :: FilePath -> (FilePath -> [FilePath] -> [FilePath] -> IO ()) -> IO ()
pathWalk root fn = do
    allContents <- getDirectoryContents root
    let contents = filter (`notElem` [".", ".."]) allContents
    putStrLn $ root ++ ": " ++ show contents

    dirs <- filterM (\p -> doesDirectoryExist $ joinPath [root, p]) contents
    files <- filterM (\p -> doesFileExist $ joinPath [root, p]) contents

    putStrLn $ "dirs: " ++ show dirs
    putStrLn $ "files: " ++ show files

    fn root dirs files

    forM_ dirs $ \dir -> do
        let combined = joinPath [root, dir]
        pathWalk combined fn

multiFileTestMain :: Q [Dec]
multiFileTestMain = do
    this_filename <- fmap loc_filename location
    runIO $ do
        testFiles <- newIORef []
        pathWalk (takeDirectory this_filename) $ \root dirs files -> do
            forM_ files $ \file -> do
                when ("Test.hs" `isSuffixOf` file) $ do
                    modifyIORef testFiles ((joinPath [root, file]):)

            putStrLn $ show (root, dirs, files)
        readIORef testFiles >>= (putStrLn . show . reverse)
    return []

collectTests :: Q Exp
collectTests = functionExtractorMap
    [ ("^test_", VarE 'testCase)
    , ("^xtest_", VarE 'testDisabled)
    , ("^prop_", VarE 'testProperty)
    , ("^xprop_", VarE 'testDisabled)
    ]
