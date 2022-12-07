{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board where

import qualified Data.Sequence as Seq
import Linear.V2 (V2 (..), _y)
import System.Random
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Tiles
import UI.Game
import UI.Difficulty

-- Initial Board State Tests
initTileTest :: TestTree
initTileTest =
  testGroup
    "Testing initialization of board"
    [ testCase "Create a orange tile" $ do
        let possibility = [[OrangeTile]]
        let orangeTarget = Seq.singleton (Tile OrangeTile orangeOrigin)
        let orangeTile = initTile (Seq.fromList (possibility !! 0)) (length (possibility !! 0) - 1)
        assertEqual "Orange does not match" orangeTile orangeTarget,
      testCase "Create a magenta tile" $ do
        let possibility = [[MagentaTile]]
        let magentaTarget = Seq.singleton (Tile MagentaTile magentaOrigin)
        let magentaTile = initTile (Seq.fromList (possibility !! 0)) (length (possibility !! 0) - 1)
        assertEqual "Magenta does not match" magentaTile magentaTarget,
      testCase "Create a random tile" $ do
        let possibilitiesTest = [[OrangeTile], [MagentaTile]]
        randomIndex <- getStdRandom (randomR (0, length possibilitiesTest - 1))
        let possibleTiles =
              [ (Seq.singleton (Tile OrangeTile orangeOrigin)),
                (Seq.singleton (Tile MagentaTile magentaOrigin))
              ]
        let randomTile = initTile (Seq.fromList (possibilitiesTest !! randomIndex)) (length (possibilitiesTest !! randomIndex) - 1)
        assertBool "Random tile exists in possibilities" (randomTile `elem` possibleTiles)
    ]

-- Stopped Tiles on the Board Tests
atBottomTest :: TestTree
atBottomTest =
  testGroup
    "Testing stopped tiles"
    [ testCase "Tile is stopped" $ do
        let coords = (V2 2 1)
        assertEqual "Tile should be stopped" True (atBottom coords),
      testCase "Tile is not stopped" $ do
        let coords = (V2 2 2)
        assertEqual "Tile should not be stopped" False (atBottom coords),
      testCase "Tile is not stopped with X = 1" $ do
        let coords = (V2 1 2)
        assertEqual "Tile should not be stopped" False (atBottom coords)
    ]

atBottomHelperTest :: TestTree
atBottomHelperTest =
  testGroup
    "Testing stopped Tiles (sequence)"
    [ testCase "Tile from sequence is stopped" $ do
        let sequence = Seq.fromList [Tile (OrangeTile) (V2 2 1), Tile (OrangeTile) (V2 4 1)]
        assertEqual "Tile should be stopped" True (atBottomHelper sequence),
      testCase "Tile from sequence is not stopped" $ do
        let sequence = Seq.fromList [Tile (OrangeTile) (V2 2 2), Tile (OrangeTile) (V2 4 2)]
        assertEqual "Tile should not be stopped" False (atBottomHelper sequence)
    ]

tileAtKeyGame :: Game
tileAtKeyGame =
  Game
    { _level = 0,
      _tiles = Seq.fromList [(Seq.singleton (Tile (OrangeTile) (V2 2 2))), Seq.singleton ((Tile (OrangeTile) (V2 2 1)))],
      _nextTile = Seq.fromList ([OrangeTile]),
      _score = 0,
      _lives = 0,
      _keyPress1 = False,
      _keyPress2 = False,
      _keyPress3 = False,
      _keyPress4 = False,
      _keyPress1Correct = False,
      _keyPress2Correct = False,
      _keyPress3Correct = False,
      _keyPress4Correct = False,
      _keyPress1Incorrect = False,
      _keyPress2Incorrect = False,
      _keyPress3Incorrect = False,
      _keyPress4Incorrect = False,
      _board = mempty
    }

tileAtKeyGameFalse :: Game
tileAtKeyGameFalse =
  Game
    { _level = 0,
      _tiles = Seq.fromList [(Seq.singleton (Tile (OrangeTile) (V2 2 2))), Seq.singleton ((Tile (OrangeTile) (V2 2 2)))],
      _nextTile = Seq.fromList ([OrangeTile]),
      _score = 0,
      _lives = 0,
      _keyPress1 = False,
      _keyPress2 = False,
      _keyPress3 = False,
      _keyPress4 = False,
      _keyPress1Correct = False,
      _keyPress2Correct = False,
      _keyPress3Correct = False,
      _keyPress4Correct = False,
      _keyPress1Incorrect = False,
      _keyPress2Incorrect = False,
      _keyPress3Incorrect = False,
      _keyPress4Incorrect = False,
      _board = mempty
    }

tileAtKeyGameFalse2 :: Game
tileAtKeyGameFalse2 =
  Game
    { _level = 0,
      _tiles = Seq.fromList [(Seq.singleton (Tile (OrangeTile) (V2 2 1))), Seq.singleton ((Tile (OrangeTile) (V2 2 2)))],
      _nextTile = Seq.fromList ([OrangeTile]),
      _score = 0,
      _lives = 0,
      _keyPress1 = False,
      _keyPress2 = False,
      _keyPress3 = False,
      _keyPress4 = False,
      _keyPress1Correct = False,
      _keyPress2Correct = False,
      _keyPress3Correct = False,
      _keyPress4Correct = False,
      _keyPress1Incorrect = False,
      _keyPress2Incorrect = False,
      _keyPress3Incorrect = False,
      _keyPress4Incorrect = False,
      _board = mempty
    }

tileAtKeyTest :: TestTree
tileAtKeyTest =
  testGroup
    "Test stopped Tiles (game)"
    [ testCase "Last Tile of game is stopped" $ do
        assertEqual "Tile should be stopped" True (tileAtKey tileAtKeyGame),
      testCase "Last Tile of game is not stopped" $ do
        assertEqual "Tile should not be stopped" False (tileAtKey tileAtKeyGameFalse),
      testCase "Last Tile of game is not stopped" $ do
        assertEqual "Tile should not be stopped" False (tileAtKey tileAtKeyGameFalse2)
    ]

boardTest :: TestTree
boardTest = testGroup "Board test" [initTileTest, atBottomTest, atBottomHelperTest, tileAtKeyTest]