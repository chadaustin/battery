{-# LANGUAGE TemplateHaskell #-}

module ThreeTest (tests) where

import Battery.Test

test_one = do
    $assertEqual 1 2

tests = $(exportTestFile)
