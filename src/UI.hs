{-# LANGUAGE OverloadedStrings #-}
module UI where

-- | Import libraries from 
import GuitarHero

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padTopBottom, padAll, Padding(..)
  , withBorderStyle
  , str 
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Col1 | Col2 | Col3 | Col4 | Key1 | Key2 | Key3 | Key4 | Empty

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ turn South g  -- added as the 'Start' button
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 18
  $ vBox [ drawScore (g ^. score)
         , drawHelp
         , padTop (Pad 2) $ drawGameOver (g ^. finished)
         , padTop (Pad 4) $ drawReadySet (g ^. paused)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver finished =
  if finished
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget
    
drawReadySet :: Bool -> Widget Name
drawReadySet paused = 
  if paused
    then withAttr readySetAttr $ C.hCenter $ str "SPACE"
    else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Guitar Hero")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. col1 = Col1
      | c `elem` g ^. col2 = Col2
      | c `elem` g ^. col3 = Col3
      | c `elem` g ^. col4 = Col4
      | c `elem` g ^. key1 = Key1
      | c `elem` g ^. key2 = Key2
      | c `elem` g ^. key3 = Key3
      | c `elem` g ^. key4 = Key4
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Col1 = withAttr col1Attr cw
drawCell Col2 = withAttr col2Attr cw
drawCell Col3 = withAttr col3Attr cw
drawCell Col4 = withAttr col4Attr cw
drawCell Key1 = withAttr col1Attr cw
drawCell Key2 = withAttr col2Attr cw
drawCell Key3 = withAttr col3Attr cw
drawCell Key4 = withAttr col4Attr cw
drawCell Empty = withAttr emptyAttr cw

drawHelp :: Widget Name
drawHelp =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Help")
    $ padTopBottom 1
    $ vBox
    $ map (uncurry drawKeyInfo)
      [ ("first"   , "z")
      , ("second"  , "x")
      , ("third"   , "n")
      , ("fourth"   , "m")
      , ("Restart", "r")
      , ("Pause"  , "p")
      , ("Quit"   , "q")
      ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (col1Attr, V.green `on` V.green)
  , (col2Attr, V.yellow `on` V.yellow)
  , (col3Attr, V.blue `on` V.blue)
  , (col4Attr, V.magenta `on` V.magenta)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (readySetAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

readySetAttr :: AttrName
readySetAttr = "readySet"

col1Attr, col2Attr, col3Attr, col4Attr, emptyAttr:: AttrName
col1Attr = "col1Attr"
col2Attr = "col2Attr"
col3Attr = "col3Attr"
col4Attr = "col4Attr"
emptyAttr = "emptyAttr"