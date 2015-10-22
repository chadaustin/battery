{-# LANGUAGE TemplateHaskell #-}

module ThreeTest (tests) where
import Battery.Test

test_two = do
    $assertEqual 2 3

tests = $(exportTestFile)
