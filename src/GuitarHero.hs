{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module GuitarHero
  ( initGame,
    step,
    turn,
    Game (..),
    Direction (..),
    score,
    col1,
    col2,
    col3,
    col4,
    paused,
    finished,
    key1,
    key2,
    key3,
    key4,
    notes,
    board,
    boards,
    height,
    width,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as SEQ
import Linear.V2
import System.Random

-- Types

data Game = Game
  { _col1 :: GuitarHero, -- column as a sequence of notes going down screen
    _col2 :: GuitarHero, -- might need streams of randomly generated
    _col3 :: GuitarHero,
    _col4 :: GuitarHero,
    _dir :: Direction, -- everything goes down (may or may not need)
    _key1 :: GuitarHero, -- position of key remains same
    _key2 :: GuitarHero,
    _key3 :: GuitarHero,
    _key4 :: GuitarHero,
    _board :: Coord,
    _boards :: Stream Coord,
    _notes :: Bool, -- infinite list of random next notes maybe
    _finished :: Bool, -- game over flag
    _paused :: Bool, -- paused flag
    _score :: Int -- score
  }
  deriving (Show)

type Coord = V2 Int

type GuitarHero = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  -- = North
  = South
  -- | East
  -- | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 20
width = 9

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use finished]

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  MaybeT (Just <$> modify move1)
  MaybeT (Just <$> modify move2)
  MaybeT (Just <$> modify move3)
  MaybeT (Just <$> modify move4)


-- | Move cell
move1 :: Game -> Game
move1 g@Game {_col1 = (s :|> _)} = g & col1 .~ (nextCell1 g <| s)
move1 _ = error "Snakes can't be empty!"

move2 :: Game -> Game
move2 g@Game {_col2 = (s :|> _)} = g & col2 .~ (nextCell2 g <| s)
move2 _ = error "Snakes can't be empty!"

move3 :: Game -> Game
move3 g@Game {_col3 = (s :|> _)} = g & col3 .~ (nextCell3 g <| s)
move3 _ = error "Snakes can't be empty!"

move4 :: Game -> Game
move4 g@Game {_col4 = (s :|> _)} = g & col4 .~ (nextCell4 g <| s)
move4 _ = error "Snakes can't be empty!"

-- | Set a valid next move coordinate
nextBoard :: State Game ()
nextBoard = do
  (b :| bs) <- use boards
  boards .= bs
  elem b <$> use col1 >>= \case
    True -> nextBoard
    False -> board .= b

-- | Get pos of next cell
nextCell1 :: Game -> Coord
nextCell1 Game {_dir = d, _col1 = (a :<| _)}
  | d == South = a & _y %~ (\y -> y - 1)
nextCell1 _ = error "Invalid move"

nextCell2 :: Game -> Coord
nextCell2 Game {_dir = d, _col2 = (a :<| _)}
  | d == South = a & _y %~ (\y -> y - 1)
nextCell2 _ = error "Invalid move"

nextCell3 :: Game -> Coord
nextCell3 Game {_dir = d, _col3 = (a :<| _)}
  | d == South = a & _y %~ (\y -> y - 1)
nextCell3 _ = error "Invalid move"


nextCell4 :: Game -> Coord
nextCell4 Game {_dir = d, _col4 = (a :<| _)}
  | d == South = a & _y %~ (\y -> y - 1)
nextCell4 _ = error "Invalid move"

turn :: Direction -> Game -> Game
turn d g = g & dir %~ turnDir d & paused .~ False

turnDir :: Direction -> Direction -> Direction
turnDir n c = c

-- | Initialize a game (paused for now because need movement)
initGame :: IO Game
initGame = do
  (b :| bs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let ykey = 1
      ycol = height  --`div` 2
      g =
        Game
        {   _col1 = SEQ.fromList([(V2 1 (ycol - 7)), (V2 1 (ycol - 2))]),
            _col2 = (SEQ.singleton (V2 3 ycol)),
            _col3 = (SEQ.singleton (V2 5 ycol)),
            _col4 = (SEQ.singleton (V2 7 ycol)),
            _key1 = (SEQ.singleton (V2 1 0)),
            _key2 = (SEQ.singleton (V2 3 0)),
            _key3 = (SEQ.singleton (V2 5 0)),
            _key4 = (SEQ.singleton (V2 7 0)),
            _notes = False,
            _board = b,
            _boards = bs,
            _score = 0,
            _dir = South,
            _finished = False,
            _paused = True
          }
  return $ execState nextBoard g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
