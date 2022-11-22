{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module GuitarHero
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , score, col1, col2, col3, col4, paused, finished
  , key1, key2, key3, key4, notes, board, boards
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Linear.V2
import System.Random
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as SEQ

-- Types

data Game = Game
  { _col1   :: GuitarHero    -- column as a sequence of notes going down screen
  , _col2   :: GuitarHero    -- might need streams of randomly generated
  , _col3   :: GuitarHero
  , _col4   :: GuitarHero
  , _dir    :: Direction    -- everything goes down (may or may not need)
  , _key1   :: GuitarHero     -- position of key remains same
  , _key2   :: GuitarHero
  , _key3   :: GuitarHero
  , _key4   :: GuitarHero
  , _board  :: Coord
  , _boards :: Stream Coord
  , _notes  :: Bool -- infinite list of random next notes maybe
  , _finished :: Bool       -- game over flag
  , _paused :: Bool         -- paused flag
  , _score  :: Int          -- score
  } deriving (Show)

type Coord = V2 Int

type GuitarHero = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)
  
data Direction
  = North
  | South
  | East
  | West
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
  MaybeT (Just <$> modify move)

-- | Move cell
move :: Game -> Game
move g@Game { _col1 = (s :|> _) } = g & col1 .~ (nextCell g <| s)
move _                             = error "Snakes can't be empty!"

-- | Set a valid next move coordinate
nextBoard :: State Game ()
nextBoard = do
  (b :| bs) <- use boards
  boards .= bs
  elem b <$> use col1 >>= \case
    True -> nextBoard
    False -> board .= b

-- | Get pos of next cell
nextCell :: Game -> Coord
nextCell Game { _dir = d, _col1 = (a :<| _) }
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)
nextCell _ = error "Snakes can't be empty!"

turn :: Direction -> Game -> Game
turn d g = g & dir %~ turnDir d & paused .~ False

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Initialize a game (paused for now because need movement)

initGame :: IO Game
initGame = do
  (b :| bs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let ykey = 1
      ycol = height - 2   --`div` 2
      g  = Game
        { _col1   = (SEQ.singleton (V2 1 ycol))
        , _col2   = (SEQ.singleton (V2 3 ycol))
        , _col3   = (SEQ.singleton (V2 5 ycol))
        , _col4   = (SEQ.singleton (V2 7 ycol))
        , _key1   = (SEQ.singleton (V2 1 1))
        , _key2   = (SEQ.singleton (V2 3 1))
        , _key3   = (SEQ.singleton (V2 5 1))
        , _key4   = (SEQ.singleton (V2 7 1))
        , _notes  = False
        , _board  = b
        , _boards = bs
        , _score  = 0
        , _dir    = North
        , _finished  = False
        , _paused = True
        }
  return $ execState nextBoard g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")