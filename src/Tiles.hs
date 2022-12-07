{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tiles where

import Control.Lens hiding (Empty)
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Control.Monad.Trans.State (StateT (..), evalStateT, execStateT, gets)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Linear.V2 (V2 (..), _y)
import System.Random (getStdRandom, randomR)

-- Types and instances
-- | Different key presses
data KeyPress = KeyPress1 | KeyPress2 | KeyPress3 | KeyPress4
  deriving (Eq, Show, Enum)
-- | Different tile types
data TileType = OrangeTile | YellowTile | CyanTile | MagentaTile | EmptyTile
  deriving (Eq, Show, Enum)

-- | Coordinates
type Coord = V2 Int

-- | Tile with column origin
data Tile = Tile
  { -- | tile type
    _tileType :: TileType,
    -- | origin
    _origin :: Coord
  }
  deriving (Eq, Show)

makeLenses ''Tile

data Direction = Down
  deriving (Eq, Show)

-- Map of coordinates with tiles at those coordinates
type Board = Map Coord TileType

-- | Game state
data Game = Game
  { _level :: Int,
    _tiles :: Seq.Seq (Seq.Seq Tile),
    _keyPress1 :: Bool,
    _keyPress2 :: Bool,
    _keyPress3 :: Bool,
    _keyPress4 :: Bool,
    _keyPress1Correct :: Bool,
    _keyPress2Correct :: Bool,
    _keyPress3Correct :: Bool,
    _keyPress4Correct :: Bool,
    _keyPress1Incorrect:: Bool,
    _keyPress2Incorrect :: Bool,
    _keyPress3Incorrect :: Bool,
    _keyPress4Incorrect :: Bool,
    _nextTile :: Seq.Seq TileType,
    _score :: Int,
    _lives :: Int,
    _board :: Board
  }
  deriving (Eq, Show)

makeLenses ''Game

type TilesT = StateT Game

type Tiles a = forall m. (Monad m) => TilesT m a

execTiles :: Tiles a -> Game -> Game
execTiles m = runIdentity . execStateT m

-- Translate class for shifting tiles in the downward direction
class Translatable s where
  translate :: Direction -> s -> s
  translate = translateBy 1
  translateBy :: Int -> Direction -> s -> s

instance Translatable Coord where
  translateBy n Down (V2 x y) = V2 x (y - n)

instance Translatable Tile where
  translateBy n d b =
    b & origin %~ translateBy n d

-- Initialize the approriate row of tiles that will fall from the top based on the Tile type
initTile :: Seq.Seq TileType -> Int -> Seq.Seq Tile
initTile s n =
  let t = Seq.index s n
   in case t of
        OrangeTile -> Seq.singleton (Tile t orangeOrigin)
        YellowTile -> Seq.singleton (Tile t yellowOrigin)
        CyanTile -> Seq.singleton (Tile t cyanOrigin)
        MagentaTile -> Seq.singleton (Tile t magentaOrigin)
        EmptyTile ->
          Seq.singleton (Tile t orangeOrigin)
            |> Tile t yellowOrigin
            |> Tile t cyanOrigin
            |> Tile t magentaOrigin

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 9
boardHeight = 20

-- | Coordinate from where orange tile falls from
orangeOrigin :: Coord
orangeOrigin = V2 2 22

-- | Coordinate from where yellow tile falls from
yellowOrigin :: Coord
yellowOrigin = V2 4 22

-- | Coordinate from where cyan tile falls from
cyanOrigin :: Coord
cyanOrigin = V2 6 22

-- | Coordinate from where magenta tile falls from
magentaOrigin :: Coord
magentaOrigin = V2 8 22

-- | Get coordinates of the tile
coords :: Tile -> Coord
coords b = b ^. origin

-- Possible rows of tiles that can be generated
possibilities :: [[TileType]]
possibilities = [[EmptyTile, EmptyTile, EmptyTile, EmptyTile], [OrangeTile], [CyanTile], [YellowTile], [MagentaTile]]

-- | Initialize a game with a given level, lives, etc.
initGame :: Int -> IO Game
initGame lvl = do
  randomIndex1 <- getStdRandom (randomR (0, length possibilities - 1))
  randomIndex2 <- getStdRandom (randomR (0, length possibilities - 1))
  pure $
    Game
      { _level = lvl,
        _tiles = Seq.fromList [initTile (Seq.fromList (possibilities !! randomIndex1)) (length (possibilities !! randomIndex1) - 1)],
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
        _nextTile = Seq.fromList (possibilities !! randomIndex2),
        _score = 0,
        _lives = 10,
        _board = mempty
      }

-- Check if the game is over (when the lives are equal to 0)
isGameOver :: Game -> Bool
isGameOver g = g ^. lives == 0

-- | The main game execution, this is executed at each discrete time step
timeStep :: MonadIO m => TilesT m ()
timeStep = do
  press1 <- use keyPress1
  updateKey KeyPress1 press1
  press2 <- use keyPress2
  updateKey KeyPress2 press2
  press3 <- use keyPress3
  updateKey KeyPress3 press3
  press4 <- use keyPress4
  updateKey KeyPress4 press4

  gets tileAtKey >>= \case
    False -> do
      gravitate
      newTile
    True -> do
      if press1 || press2 || press3 || press4
        then do
          score %= (+ 1)
          clearRow
          keyPress1 .= False
          keyPress2 .= False
          keyPress3 .= False
          keyPress4 .= False
          keyPress1Incorrect .= False
          keyPress2Incorrect .= False
          keyPress3Incorrect .= False
          keyPress4Incorrect .= False
          gravitate
          newTile
        else do
          gets lastBlockTileType >>= \case
            OrangeTile -> do 
              keyPress1Incorrect .= True
              keyPress2Incorrect .= False
              keyPress3Incorrect .= False
              keyPress4Incorrect .= False
            YellowTile -> do
              keyPress1Incorrect .= False
              keyPress2Incorrect .= True
              keyPress3Incorrect .= False
              keyPress4Incorrect .= False
            CyanTile -> do 
              keyPress1Incorrect .= False
              keyPress2Incorrect .= False
              keyPress3Incorrect .= True
              keyPress4Incorrect .= False
            MagentaTile -> do
              keyPress1Incorrect .= False
              keyPress2Incorrect .= False
              keyPress3Incorrect .= False
              keyPress4Incorrect .= True
            _ -> do
                  keyPress1Incorrect .= False
                  keyPress2Incorrect .= False
                  keyPress3Incorrect .= False
                  keyPress4Incorrect .= False
          loseLife
          clearRow
          gravitate
          newTile

-- Get the tile type of the last row in the board tiles.
lastBlockTileType :: Game -> TileType
lastBlockTileType g = Seq.index (Seq.index (g ^. tiles) (length (g ^. tiles) - 1)) 0 ^. tileType

-- Check if a tile has reached the keys.
tileAtKey :: Game -> Bool
tileAtKey g = outerIsStopped (g ^. tiles)

outerIsStopped :: Seq.Seq (Seq.Seq Tile) -> Bool
outerIsStopped xs = not (null xs) && atBottomHelper (Seq.index xs (length xs - 1))

atBottomHelper :: Seq.Seq Tile -> Bool
atBottomHelper blks = atBottom (coords (Seq.index blks 0))

atBottom :: Coord -> Bool
atBottom x = x ^. _y == 1

-- | Shift tiles down
gravitate :: Tiles ()
gravitate = shift Down

-- Clear row of tiles once it has reached the keys
clearRow :: Tiles ()
clearRow = do
            currentTiles <- use tiles
            tiles .= Seq.deleteAt (length currentTiles - 1) currentTiles

-- | Replace tile with next tile
newTile :: MonadIO m => TilesT m ()
newTile = do
  randomIndex <- liftIO $ getStdRandom (randomR (0, length possibilities - 1))
  currentTiles <- use tiles
  use nextTile >>= \s -> tiles .= initTile s (length s - 1) <| currentTiles
  nextTile .= Seq.fromList (possibilities !! randomIndex)

-- Update score of the game if a tile is correctly pressed
updateScore :: Coord -> Tiles ()
updateScore c = do
  currentTiles <- use tiles
  if length currentTiles /= 0
    then
      if ((Seq.index (Seq.index currentTiles (length currentTiles - 1)) 0) ^. tileType /= EmptyTile)
        && ((Seq.index (Seq.index currentTiles (length currentTiles - 1)) 0) ^. origin == c)
        then 
          case ((Seq.index (Seq.index currentTiles (length currentTiles - 1)) 0) ^. tileType) of
            OrangeTile -> keyPress1 .= True
            YellowTile -> keyPress2 .= True
            CyanTile -> keyPress3 .= True
            MagentaTile -> keyPress4 .= True
            _ -> do
              keyPress1 .= False
              keyPress2 .= False
              keyPress3 .= False
              keyPress4 .= False
        else return ()
    else return ()

-- Used to keep track of when key is pressed
updateKey :: KeyPress -> Bool -> Tiles ()
updateKey kp b = case kp of
                    KeyPress1 -> keyPress1Correct .= b
                    KeyPress2 -> keyPress2Correct .= b
                    KeyPress3 -> keyPress3Correct .= b
                    KeyPress4 -> keyPress4Correct .= b

-- Lose a life if a tile is missed when it reaches the keys
loseLife :: Tiles ()
loseLife = do
  currentTiles <- use tiles
  if length currentTiles /= 0
    then
      if (Seq.index (Seq.index currentTiles (length currentTiles - 1)) 0) ^. tileType == EmptyTile
        then return ()
        else do
          lives %= (+ (-1))
          return ()
    else return ()

-- | Shift the current tile
shift :: Direction -> Tiles ()
shift dir = do
  blk <- use tiles
  tiles .= outerTranslateHelper dir blk

outerTranslateHelper :: Direction -> Seq (Seq Tile) -> Seq (Seq Tile)
outerTranslateHelper dir seqBlks =
  if length seqBlks == 0
    then seqBlks
    else innerTranslateHelper dir seqBlks (length seqBlks - 1)

innerTranslateHelper :: Direction -> Seq (Seq Tile) -> Int -> Seq (Seq Tile)
innerTranslateHelper dir seqBlks 0 = Seq.singleton (translateHelper dir (Seq.index seqBlks 0) (length (Seq.index seqBlks 0) - 1))
innerTranslateHelper dir seqBlks n = innerTranslateHelper dir seqBlks (n - 1) |> translateHelper dir (Seq.index seqBlks n) (length (Seq.index seqBlks n) - 1)

translateHelper :: Direction -> Seq Tile -> Int -> Seq Tile
translateHelper dir b 0 = Seq.singleton (translate dir (Seq.index b 0))
translateHelper dir b n = translate dir (Seq.index b n) <| translateHelper dir b (n - 1)