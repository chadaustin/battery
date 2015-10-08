{-# LANGUAGE TemplateHaskell #-}

import Battery.Test

test_foo = do
    $assert $ Equal 1 2

testMain
