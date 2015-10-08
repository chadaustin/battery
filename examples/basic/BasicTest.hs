{-# LANGUAGE TemplateHaskell #-}

import Battery.Test

test_zzz = do
    $assert $ Equal 0 0

test_foo = do
    $assert $ Equal 1 2

{-
test_commented_out = do
    $assert $ Equal 1 2
-}

testMain
