{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Test.QuickCheck
import Test.Tasty.HUnit

import Board (boardTest)
import Logic (logicTest)

import UI.Game
import UI.Difficulty
import Tiles

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test Suite" [boardTest, logicTest]