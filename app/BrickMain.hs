module BrickMain where

import Brick.Main (App(..), defaultMain, resizeOrQuit, suspendAndResume', neverShowCursor)
import Brick.Types (Widget, BrickEvent(..), EventM, get, put, CursorLocation(..), Location(..))
import Brick.Widgets.Core (str, clickable, (<+>), (<=>), vBox, withAttr, showCursor)
import Brick.Widgets.Border (border, hBorder)
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Util (fg)

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Input.Events (Key(..), Modifier(..), Event(..), Button(..))

import Data.List (intersperse)
import Data.Char (isDigit)

import qualified GlossMain as GM

import qualified Debug.Trace as DT

{-
The stats data type which holds
all of the values on the screen as well
as a list of all the input widget names
and which one is currently selected.
-}
data MyState = MyState
    { _timesTableNum :: String
    , _pointsAroundCircle :: String
    , _windowSize :: String
    , _stepSize :: String
    , _framesPerSecond :: String
    , _currentFocus :: WidgetName
    , _inputs :: [WidgetName]
    , _curInputNum :: Int
    } deriving (Eq, Show)

initialState :: MyState
initialState =
    let n = 0
    in MyState "" "" "" "" "" (inputsList !! n) inputsList n

{-
Data type which allows us to label each input
line in the UI in order to more easily switch
between them.
-}
data WidgetName =
    TimesTableNum | PointsAroundCircle |
    WindowSize | StepSize |
    FramesPerSecond
    deriving (Eq, Ord, Show, Enum)

inputsList :: [WidgetName]
inputsList = [ TimesTableNum .. FramesPerSecond ]

labelAttr :: AttrName
labelAttr = attrName "label"

inputAttr :: AttrName
inputAttr = attrName "input"

-- Draw an input line as a label to the left of an input field
drawInputRow :: WidgetName -> String -> Widget WidgetName
drawInputRow label value =
    withAttr labelAttr (str (show label ++ ": ")) <+>
    withAttr inputAttr (str value)

drawUI :: MyState -> [Widget WidgetName]
drawUI s =
    [ vBox $ intersperse hBorder [
        drawInputRow TimesTableNum (_timesTableNum s),
        drawInputRow PointsAroundCircle (_pointsAroundCircle s),
        drawInputRow WindowSize (_windowSize s),
        drawInputRow StepSize (_stepSize s),
        drawInputRow FramesPerSecond (_framesPerSecond s)
    ]]

handleChar :: WidgetName -> Char -> MyState -> MyState
handleChar name c s = case name of
    TimesTableNum      -> s { _timesTableNum = _timesTableNum s ++ [c] }
    PointsAroundCircle -> s { _pointsAroundCircle = _pointsAroundCircle s ++ [c] }
    WindowSize         -> s { _windowSize = _windowSize s ++ [c] }
    StepSize           -> s { _stepSize = _stepSize s ++ [c] }
    FramesPerSecond    -> s { _framesPerSecond = _framesPerSecond s ++ [c] }
    _                  -> s

removeLast :: [a] -> [a]
removeLast [] = []
removeLast xs = init xs

handleBackspace :: WidgetName -> MyState -> MyState
handleBackspace name s = case name of
    TimesTableNum      -> s { _timesTableNum = removeLast $ _timesTableNum s }
    PointsAroundCircle -> s { _pointsAroundCircle = removeLast $ _pointsAroundCircle s }
    WindowSize         -> s { _windowSize = removeLast $ _windowSize s }
    StepSize           -> s { _stepSize = removeLast $ _stepSize s }
    FramesPerSecond    -> s { _framesPerSecond = removeLast $ _framesPerSecond s }
    _                  -> s

changeCurInputNum :: Int -> Int -> Int -> Int
changeCurInputNum size by cin
    | res >= size = 0
    | res < 0     = size - 1
    | otherwise   = res
    where
        res = cin + by

fromString :: Read a => a -> String -> a
fromString def []  = def
fromString def str = read str

stateToInput :: MyState -> (Int, Int, Float, Int, Float)
stateToInput s = (ws, fps, ttn, poc, ss)
    where
        ws  = fromString 600 (_windowSize s)
        fps = fromString 15  (_framesPerSecond s)
        ttn = fromString 2.0 (_timesTableNum s)
        poc = fromString 360 (_pointsAroundCircle s)
        ss  = fromString 0.1 (_stepSize s)

getTabOffSet :: Key -> Int
getTabOffSet KBackTab = -1
getTabOffSet _        = 1

shiftFocus :: Key -> MyState -> MyState
shiftFocus key s =
    let nextInputNum = 
            changeCurInputNum (length (_inputs s)) (getTabOffSet key) (_curInputNum s)
    in s { _currentFocus = _inputs s !! nextInputNum,
           _curInputNum  =  nextInputNum }

-- Concise but complex version
handleEvent bevent@(VtyEvent (EvKey key mods)) = do
    s <- get
    case key of
        KChar '\t' -> put $ shiftFocus key s
        KBackTab   -> put $ shiftFocus key s
        KBS        -> put (handleBackspace (_currentFocus s) s)
        KEnter     -> suspendAndResume' (GM.run $ stateToInput s)
        KChar 'q'  -> resizeOrQuit bevent
        KChar c    ->
            if isDigit c || c == '.'
            then put (handleChar (_currentFocus s) c s)
            else return ()
        _          -> return () 
handleEvent bevent = return ()

-- Verbose but simple version
-- handleEvent (VtyEvent (EvKey (KChar '\t') mods)) = do
--     s <- get
--     let nextInputNum = changeCurInputNum (length (_inputs s)) 1 (_curInputNum s)
--     put (s { _currentFocus = _inputs s !! nextInputNum,
--              _curInputNum =  nextInputNum })
-- handleEvent (VtyEvent (EvKey KBackTab mods)) = do
--     s <- get
--     let nextInputNum = changeCurInputNum (length (_inputs s)) (-1) (_curInputNum s)
--     put (s { _currentFocus = _inputs s !! nextInputNum,
--              _curInputNum =  nextInputNum })
-- handleEvent (VtyEvent (EvKey KBS mods)) = do
--     s <- get
--     put (handleBackspace (_currentFocus s) s)
-- handleEvent bevent@(VtyEvent (EvKey (KChar 'q') mods)) =
--     resizeOrQuit bevent
-- handleEvent (VtyEvent (EvKey (KChar c) mods))
--     | isDigit c || c == '.' = do
--         s <- get
--         put (handleChar (_currentFocus s) c s)
--     | otherwise = return ()
-- handleEvent (VtyEvent (EvKey KEnter mods)) = do
--     s <- get
--     suspendAndResume' (GM.run $ stateToInput s)
-- handleEvent bevent = return ()

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (labelAttr, fg V.blue),
      (inputAttr, fg V.black) ]

app :: App MyState () WidgetName
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

run :: IO ()
run = do
    defaultMain app initialState
    return ()