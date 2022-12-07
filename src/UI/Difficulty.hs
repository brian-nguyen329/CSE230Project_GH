module UI.Difficulty
  ( difficulty,
  )
where

import Brick
  ( App (..),
    BrickEvent (VtyEvent),
    EventM,
    Next,
    Padding (Pad),
    Widget,
    attrMap,
    continue,
    defaultMain,
    hLimit,
    halt,
    neverShowCursor,
    padLeft,
    padRight,
    str,
    vLimit,
    withBorderStyle,
    hBox
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Exit (exitSuccess)

app :: App (Maybe Int) e ()
app =
  App
    { appDraw = const [ui],
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = neverShowCursor
    }

ui :: Widget ()
ui =
  padLeft (Pad 19) $
    padRight (Pad 21) $
      C.center $
        vLimit 22 $
          hLimit 22 $
            withBorderStyle BS.unicodeBold $
              B.borderWithLabel (str "Tiles") $
                C.center $
                  str "Choose Difficulty: \n Easy   (1) \n Medium (2) \n Hard   (3)"

handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent n (VtyEvent (V.EvKey V.KEsc _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar '1') [])) = halt $ Just (read ['0'])
handleEvent n (VtyEvent (V.EvKey (V.KChar '2') [])) = halt $ Just (read ['2'])
handleEvent n (VtyEvent (V.EvKey (V.KChar '3') [])) = halt $ Just (read ['3'])
handleEvent n _ = continue n

difficulty :: IO Int
difficulty = defaultMain app Nothing >>= maybe exitSuccess return
