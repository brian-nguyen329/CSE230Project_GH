{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic where

import Control.Lens hiding (Empty)
import qualified Data.Sequence as Seq
import Linear.V2 (V2 (..), _y)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tiles
import UI.Game
import UI.Difficulty
import Prelude hiding (Left, Right)
import qualified Graphics.Vty as V
import Brick hiding (Down)

-- Game instances for test
updateKeyPress :: Game
updateKeyPress =
  Game
    { _level = 0,
      _tiles = Seq.fromList [initTile (Seq.fromList ([OrangeTile])) (0)],
      _nextTile = Seq.fromList ([OrangeTile]),
      _score = 0,
      _lives = 3,
      _keyPress1 = True,
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

gameIsNotOver :: Game
gameIsNotOver =
  Game
    { _level = 0,
      _tiles = Seq.fromList [initTile (Seq.fromList ([OrangeTile])) (0)],
      _nextTile = Seq.fromList ([OrangeTile]),
      _score = 0,
      _lives = 3,
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

globalGame :: IO Game
globalGame = initGame 0

gameIsOver :: Game
gameIsOver =
  Game
    { _level = 0,
      _tiles = Seq.fromList [initTile (Seq.fromList ([OrangeTile])) (0)],
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

-- Tests
updateScoreTest :: TestTree
updateScoreTest =
  testGroup
    "Testing updating the score"
    [ testCase "Update keyPress1 should fail" $ do
        let globalGame ^. _keyPress1 = (updateScore (V2 2 2))
        game <- globalGame
        assertEqual "keyPress should be false" False (_keyPress1 game),
      testCase "Update keyPress1 should pass" $ do
        let updateKeyPress ^. _keyPress1 = (updateScore (V2 2 1))
        assertEqual "keyPress should be true" True (_keyPress1 updateKeyPress)
    ]

gameOverTest :: TestTree
gameOverTest =
  testGroup
    "Testing game over condition"
    [ testCase "Game is not over" $ do
        let bool = isGameOver gameIsNotOver
        assertEqual "Game condition not correct" False bool,
      testCase "Game is over" $ do
        let bool = isGameOver gameIsOver
        assertEqual "Game condition not correct" True bool
    ]

outerTileMapHelperTest :: TestTree
outerTileMapHelperTest = testGroup "Testing outerTileMapHelper"
    [
        testCase "Return proper [(Coords, Widget Name)]" $ do
        let seq = Seq.fromList [Seq.singleton (Tile (OrangeTile) (V2 2 2)), Seq.singleton (Tile (OrangeTile) (V2 1 1))]
        let list = outerTileMapHelper seq
        let target = Seq.fromList [Seq.singleton (V2 2 2, (withAttr (tToAttr OrangeTile) ecw)), Seq.singleton (V2 2 2, (withAttr (tToAttr OrangeTile) ecw))]
        assertEqual "Final list should match" (length list) (length target)
    ]

outerTranslateHelperTest :: TestTree
outerTranslateHelperTest = testGroup "Testing outerTranslateHelperTest"
    [
      testCase "Return proper shifted sequence" $ do
      let seq = Seq.fromList [Seq.singleton (Tile (OrangeTile) (V2 2 2)), Seq.singleton (Tile (OrangeTile) (V2 2 2))]
      let translate = outerTranslateHelper Down seq
      let target = Seq.fromList [Seq.singleton (Tile (OrangeTile) (V2 2 1)), Seq.singleton (Tile (OrangeTile) (V2 2 1))]
      assertEqual "Sequence should be shifted down" translate target
    ]



logicTest :: TestTree
logicTest = testGroup "Logic test" [updateScoreTest, gameOverTest, outerTileMapHelperTest, outerTranslateHelperTest]