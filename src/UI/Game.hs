{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module UI.Game where

import Brick hiding (Down)
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens hiding (op, preview)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))
import Tiles
import Euterpea
import Prelude hiding (Left, Right)

star = c 4 qn :+: c 4 qn :+: g 4 qn :+: g 4 qn :+:
        a 4 qn :+: a 4 qn :+: g 4 hn :+:
        f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+:
        d 4 qn :+: d 4 qn :+: c 4 hn :+:
        g 4 qn :+: g 4 qn :+: f 4 qn :+: f 4 qn :+:
        e 4 qn :+: e 4 qn :+: d 4 hn :+:
        g 4 qn :+: g 4 qn :+: f 4 qn :+: f 4 qn :+:
        e 4 qn :+: e 4 qn :+: d 4 hn :+:
        c 4 qn :+: c 4 qn :+: g 4 qn :+: g 4 qn :+:
        a 4 qn :+: a 4 qn :+: g 4 hn :+:
        f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+:
        d 4 qn :+: d 4 qn :+: c 4 hn

mary = e 4 qn :+: d 4 qn :+: c 4 qn :+: d 4 qn :+:
        e 4 qn :+: e 4 qn :+: e 4 hn :+:
        d 4 qn :+: d 4 qn :+: d 4 hn :+:
        e 4 qn :+: g 4 qn :+: g 4 hn :+:
        e 4 qn :+: d 4 qn :+: c 4 qn :+: d 4 qn :+:
        e 4 qn :+: e 4 qn :+: e 4 qn :+: e 4 qn :+:
        d 4 qn :+: d 4 qn :+: e 4 qn :+: d 4 qn :+:
        c 4 wn

data UI = UI
  { -- | tetris game
    _game :: Game,
    _locked :: Bool,
    -- | game
    _paused :: Bool
  }

makeLenses ''UI

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

-- App definition and execution

app :: App UI Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

playGame ::
  -- | Starting level
  Int ->
  -- | Preview cell (Nothing == no preview)
  IO Game
playGame lvl = do
  let delay = levelToDelay lvl
  chan <- newBChan 10
  void . forkIO $
    Control.Monad.forever $ do
      writeBChan chan Tick
      threadDelay delay
  initialGame <- initGame lvl
  forkIO $ playMusicLoop
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  ui <-
    customMain initialVty builder (Just chan) UI.Game.app $
      UI
        { _game = initialGame,
          _locked = False,
          _paused = False
        }
  return $ ui ^. game

levelToDelay :: Int -> Int
levelToDelay n = floor $ 400000 * (0.85 :: Double) ^ (2 * n)

playMusicLoop = do
                  play star
                  play mary
                  playMusicLoop

-- Handling events
handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui (AppEvent Tick) = handleTick ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'd') [])) = exec (updateScore (V2 2 1)) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'f') [])) = exec (updateScore (V2 4 1)) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'j') [])) = exec (updateScore (V2 6 1)) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'k') [])) = exec (updateScore (V2 8 1)) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'p') [])) = 
  guarded
    (not . view locked)
    (over paused not)
    ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'r') [])) = restart ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleEvent ui (VtyEvent (V.EvKey V.KEsc [])) = halt ui
handleEvent ui _ = continue ui

-- | This common execution function is used for all game user input except pause. If paused do nothing, else
-- execute the state computation.
exec :: Tiles () -> UI -> EventM Name (Next UI)
exec op =
  guarded
    (not . \ui -> ui ^. paused)
    (game %~ execTiles op)

-- | This base execution function takes a predicate and only issues UI
-- modification when predicate passes and game is not over.
guarded :: (UI -> Bool) -> (UI -> UI) -> UI -> EventM Name (Next UI)
guarded p f ui =
  continue $
    if not (p ui) || ui ^. game . to isGameOver
      then ui
      else f ui

-- | Handles time steps, does nothing if game is over or paused
handleTick :: UI -> EventM Name (Next UI)
handleTick ui =
  if ui ^. paused || ui ^. game . to isGameOver
    then continue ui
    else do
      next <- execStateT timeStep $ ui ^. game
      continue $
        ui & game .~ next
          & locked .~ False

-- | Restart game at the same level
restart :: UI -> EventM Name (Next UI)
restart ui = do
  let lvl = ui ^. (game . level)
  g <- liftIO $ initGame lvl
  continue $
    ui & game .~ g
      & locked .~ False

-- Drawing
drawUI :: UI -> [Widget Name]
drawUI ui =
  [ C.vCenter $
      vLimit 22 $
        hBox
          [ padLeft Max $ padRight (Pad 2) $ drawStats (ui ^. game),
            drawGrid ui,
            padRight Max $ padLeft (Pad 2) $ drawInfo (ui ^. game)
          ]
  ]

-- Helper to get list of (Coord, Widget Name) for each tile
outerTileMapHelper :: Seq.Seq (Seq.Seq Tile) -> [(Coord, Widget Name)]
outerTileMapHelper seqBlks =
  if length seqBlks == 0
    then []
    else innerTileMapHelper seqBlks (length seqBlks - 1)

innerTileMapHelper :: Seq.Seq (Seq.Seq Tile) -> Int -> [(Coord, Widget Name)]
innerTileMapHelper seqBlks 0 = tileMapHelper (Seq.index seqBlks 0) (length (Seq.index seqBlks 0) - 1)
innerTileMapHelper seqBlks n = tileMapHelper (Seq.index seqBlks n) (length (Seq.index seqBlks n) - 1) ++ innerTileMapHelper seqBlks (n - 1)

tileMapHelper :: Seq.Seq Tile -> Int -> [(Coord, Widget Name)]
tileMapHelper s 0 = [(coords (Seq.index s 0), drawTile (Seq.index s 0 ^. tileType))]
tileMapHelper s n = [(coords (Seq.index s n), drawTile (Seq.index s n ^. tileType))]

drawGrid :: UI -> Widget Name
drawGrid ui =
  hLimit 22 $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "Tiles") $
        case ui ^. paused of
          True -> C.center $ str "Paused"
          False ->
            vBox $
              [boardHeight, boardHeight - 1 .. 1] <&> \r ->
                foldr (Brick.<+>) emptyWidget
                  . M.filterWithKey (\(V2 _ y) _ -> r == y)
                  $ mconcat
                    [ if ui ^. (game . keyPress1Correct)
                        then M.insert (V2 2 1) correctKeyPressCellW keyCellMap
                        else
                          if ui ^. (game . keyPress2Correct)
                            then M.insert (V2 4 1) correctKeyPressCellW keyCellMap
                            else
                              if ui ^. (game . keyPress3Correct)
                                then M.insert (V2 6 1) correctKeyPressCellW keyCellMap
                                else
                                  if ui ^. (game . keyPress4Correct)
                                    then M.insert (V2 8 1) correctKeyPressCellW keyCellMap
                                    else
                                      if ui ^. (game . keyPress1Incorrect)
                                        then M.insert (V2 2 1) incorrectKeyPressCellW keyCellMap
                                        else
                                          if ui ^. (game . keyPress2Incorrect)
                                            then M.insert (V2 4 1) incorrectKeyPressCellW keyCellMap
                                            else
                                              if ui ^. (game . keyPress3Incorrect)
                                                then M.insert (V2 6 1) incorrectKeyPressCellW keyCellMap
                                                else
                                                  if ui ^. (game . keyPress4Incorrect)
                                                    then M.insert (V2 8 1) incorrectKeyPressCellW keyCellMap
                                                    else
                                                      keyCellMap,
                      drawTile <$> ui ^. (game . board),
                      tileMap (ui ^. (game . tiles)),
                      emptyCellMap
                    ]
  where
    tileMap b = M.fromList $ outerTileMapHelper b

keyCellMap :: Map Coord (Widget Name)
keyCellMap = M.fromList [(V2 2 1, keyCellW), (V2 4 1, keyCellW), (V2 6 1, keyCellW), (V2 8 1, keyCellW)]

emptyCellMap :: Map Coord (Widget Name)
emptyCellMap =
  M.fromList
    [(V2 x y, emptyNextShapeCellW) | x <- [1 .. boardWidth], y <- [1 .. boardHeight]]

emptyNextShapeCellW :: Widget Name
emptyNextShapeCellW = withAttr emptyAttr ecw

keyCellW :: Widget Name
keyCellW = withAttr keyAttr ecw

correctKeyPressCellW :: Widget Name
correctKeyPressCellW = withAttr correctKeyPressAttr ecw

incorrectKeyPressCellW :: Widget Name
incorrectKeyPressCellW = withAttr incorrectKeyPressAttr ecw

drawTile :: TileType -> Widget Name
drawTile EmptyTile = emptyNextShapeCellW
drawTile t = withAttr (tToAttr t) ecw

tToAttr :: TileType -> AttrName
tToAttr OrangeTile = orangeTileAttr
tToAttr YellowTile = yellowTileAttr
tToAttr CyanTile = cyanTileAttr
tToAttr _ = magentaTileAttr

ecw :: Widget Name
ecw = str "  "

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 22 $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "Stats") $
        vBox
          [ drawStat "Score" $ g ^. score,
            padTop (Pad 1) $ drawDifficulty "Difficulty" $ intToText (g ^. level),
            padTop (Pad 1) $ drawStat "Lives" $ g ^. lives
          ]

intToText :: Int -> String
intToText 0 = "Easy"
intToText 2 = "Medium"
intToText 3 = "Hard"
intToText _ = ""

drawDifficulty :: String -> String -> Widget Name
drawDifficulty s d = padLeftRight 1 $ str s Brick.<+> padLeft Max (str $ d)

drawStat :: String -> Int -> Widget Name
drawStat s n = padLeftRight 1 $ str s Brick.<+> padLeft Max (str $ show n)

drawInfo :: Game -> Widget Name
drawInfo g =
  hLimit 33 $
    vBox
      [ drawHelp,
        padTop (Pad 1) (drawGameOver g)
      ]

-- Draw help interface
drawHelp :: Widget Name
drawHelp =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Help") $
      padTopBottom 1 $
        vBox $
          map
            (uncurry drawKeyInfo)
            [ ("Orange  (col 1)", "d"),
              ("Yellow  (col 2)", "f"),
              ("Cyan    (col 3)", "j"),
              ("Magenta (col 4)", "k"),
              ("Restart", "r"),
              ("Pause", "p"),
              ("Quit", "q")
            ]

-- Draw information about the controls
drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    Brick.<+> padLeft Max (padRight (Pad 1) $ str keys)

-- Draw text when the game is over
drawGameOver :: Game -> Widget Name
drawGameOver g =
  if isGameOver g
    then padLeftRight 4 $ withAttr gameOverAttr $ str "GAME OVER"
    else emptyWidget

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (orangeTileAttr, tToColor OrangeTile `on` tToColor OrangeTile),
      (yellowTileAttr, tToColor YellowTile `on` tToColor YellowTile),
      (cyanTileAttr, tToColor CyanTile `on` tToColor CyanTile),
      (magentaTileAttr, tToColor MagentaTile `on` tToColor MagentaTile),
      (keyAttr, V.white `on` V.white),
      (correctKeyPressAttr, V.green `on` V.green),
      (incorrectKeyPressAttr, V.red `on` V.red),
      (gameOverAttr, fg V.red `V.withStyle` V.bold)
    ]

tToColor :: TileType -> V.Color
tToColor OrangeTile = V.rgbColor 250 165 0
tToColor YellowTile = V.brightYellow
tToColor CyanTile = V.cyan
tToColor _ = V.magenta

orangeTileAttr, yellowTileAttr, cyanTileAttr, magentaTileAttr :: AttrName
orangeTileAttr = "OrangeTile"
yellowTileAttr = "YellowTile"
cyanTileAttr = "CyanTile"
magentaTileAttr = "MagentaTile"

emptyAttr :: AttrName
emptyAttr = "empty"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

keyAttr :: AttrName
keyAttr = "key"

correctKeyPressAttr :: AttrName
correctKeyPressAttr = "correctKeyPress"

incorrectKeyPressAttr :: AttrName
incorrectKeyPressAttr = "incorrectKeyPress"