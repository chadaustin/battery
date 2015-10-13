{-# LANGUAGE TemplateHaskell #-}

import Data.Word
import Battery.Test

test_zzz = do
    $assert $ Equal 0 0
    $assertEqual 0 0

test_foo = do
    $assert $ Equal 1 2

{-
test_commented_out = do
    $assert $ Equal 1 2
-}

xtest_disabled = do
    $assert $ Equal 3 4

prop_sometimes_failing :: Word8 -> Bool
prop_sometimes_failing u = u < 255

testMain
