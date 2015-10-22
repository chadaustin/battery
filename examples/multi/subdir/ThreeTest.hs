{-# LANGUAGE TemplateHaskell #-}

module ThreeTest (tests) where

import Battery.Test

test_three = do
    $assertEqual 3 4

tests = $(exportTestFile)
