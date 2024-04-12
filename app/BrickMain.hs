module BrickMain where

import Brick.Main (App(..), defaultMain, resizeOrQuit, suspendAndResume')
import Brick.Types (Widget, BrickEvent(..), EventM, get, put, CursorLocation(..), Location(..))
import Brick.Widgets.Core (str, clickable, (<+>), (<=>), vBox, withAttr, showCursor, translateBy)
import Brick.Widgets.Table (table, renderTable)
import Brick.Widgets.Border (border, hBorder)
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Util (fg)

import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Graphics.Vty.Input.Events (Key(..), Modifier(..), Event(..), Button(..))

import Data.List (intersperse)
import Data.Char (isDigit)
import Data.Maybe (fromJust)

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
    , _curInputNum :: Int
    } deriving (Eq, Show)

initialState :: MyState
initialState =
    let n = 0
    in MyState "" "" "" "" "" (inputsList !! n) n

{-
Data type which allows us to label each input
line in the UI in order to more easily switch
between them.
-}
data WidgetName =
    TimesTableNum | PointsAroundCircle |
    WindowSize | StepSize |
    FramesPerSecond
    deriving (Eq, Ord, Show)

{-
Association list between the widget names
and their corresponding getter function defined
in MyState.
-}
nameToGetter :: [(WidgetName, MyState -> String)]
nameToGetter = 
    [ (TimesTableNum, _timesTableNum)
    , (PointsAroundCircle, _pointsAroundCircle)
    , (WindowSize, _windowSize)
    , (StepSize, _stepSize)
    , (FramesPerSecond, _framesPerSecond)
    ]

-- List of all the input widget names
inputsList :: [WidgetName]
inputsList = map fst nameToGetter

labelAttr :: AttrName
labelAttr = attrName "label"

inputAttr :: AttrName
inputAttr = attrName "input"

{-
Draw an input line as a label to the left of an input field (<+>).
We then use showCursor on the input field to request the cursor be
shown here. Where the cursor ends up is then decided by placeCursor.
Note that the Location is relative to the beginning of the widget.
-}
drawInputRow :: WidgetName -> String -> Widget WidgetName
drawInputRow label value =
    withAttr labelAttr (str (show label ++ ": ")) <+>
    showCursor label (Location (0, 0)) (withAttr inputAttr (str value))

drawControls :: Widget WidgetName
drawControls = renderTable $ table
    [ [ str "tab"          , str "shift focus down one line" ]
    , [ str "shift-tab"    , str "shift focus up one line" ]
    , [ str "backspace"    , str "remove the last character from the currently focused line" ]
    , [ str "enter"        , str "submit the current values to gloss"]
    , [ str "q"            , str "quit"]
    , [ str "digit or '.'" , str "place the digit or '.' on the end of the currently focused line"]
    , [ str "anything else", str "do nothing"]
    ]

{-
Finally we can draw our whole UI!
This is simply drawing each input row and
then putting a horizontal line between each.
-}
drawUI :: MyState -> [Widget WidgetName]
drawUI s =
    [ vBox $ intersperse hBorder $ 
        ((map go nameToGetter) ++ [drawControls]) 
    ]
    where
        go (wn, get) = drawInputRow wn (get s)

{-
This function decides where the cursor should at any given moment.
I decide this based on the currently focused widget. Whichever one
that is gets the cursor placed at the end of any input that may be there.
-}
placeCursor :: MyState -> [CursorLocation WidgetName] -> Maybe (CursorLocation WidgetName)
placeCursor s cls = case filter pred cls of
    [] -> Nothing
    (cl:_) -> 
        let get = fromJust (lookup (fromJust (cursorLocationName cl)) nameToGetter)
        in Just (cl { cursorLocation = Location (length (get s), 0) <> cursorLocation cl })
    where
        pred cl = case cursorLocationName cl of
            Nothing -> False
            Just wn -> wn == _currentFocus s

-- Can you see why we can't quite abstract this with nameToGetter?
{-
Place the given char at the end of the input of the given
widget names respective String.
-}
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

-- Can you see why we can't quite abstract this with nameToGetter?
{-
Remove the last character from the given widgets respective
String.
-}
handleBackspace :: WidgetName -> MyState -> MyState
handleBackspace name s = case name of
    TimesTableNum      -> s { _timesTableNum = removeLast $ _timesTableNum s }
    PointsAroundCircle -> s { _pointsAroundCircle = removeLast $ _pointsAroundCircle s }
    WindowSize         -> s { _windowSize = removeLast $ _windowSize s }
    StepSize           -> s { _stepSize = removeLast $ _stepSize s }
    FramesPerSecond    -> s { _framesPerSecond = removeLast $ _framesPerSecond s }
    _                  -> s

-- Increment the current input number wrapping around if necessary
changeCurInputNum :: Int -> Int -> Int -> Int
changeCurInputNum size by cin
    | res >= size = 0
    | res < 0     = size - 1
    | otherwise   = res
    where
        res = cin + by

-- Read in a String with a default value if it is empty
fromString :: Read a => a -> String -> a
fromString def []  = def
fromString def str = read str

-- Produce the input to gloss from our state,
-- with defaults
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

-- When tab is pressed shift the focus up or down
shiftFocus :: Key -> MyState -> MyState
shiftFocus key s =
    let nextInputNum = 
            changeCurInputNum (length inputsList) (getTabOffSet key) (_curInputNum s)
    in s { _currentFocus = inputsList !! nextInputNum,
           _curInputNum  =  nextInputNum }

-- Concise but complex version
{-
The main event this is where most of the behavior is
defined. The controls are:
key          : action
tab          : shift focus down one line
shift-tab    : shift focus up one line
backspace    : remove the last character from the currently focused line
enter        : submit the current values to gloss
q            : quit
digit or '.' : place the digit or '.' on the end of the currently focused line
anything else: do nothing
-}
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
--     let nextInputNum = changeCurInputNum (length inputsList) 1 (_curInputNum s)
--     put (s { _currentFocus = inputsList !! nextInputNum,
--              _curInputNum =  nextInputNum })
-- handleEvent (VtyEvent (EvKey KBackTab mods)) = do
--     s <- get
--     let nextInputNum = changeCurInputNum (length inputsList) (-1) (_curInputNum s)
--     put (s { _currentFocus = inputsList !! nextInputNum,
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
  , appChooseCursor = placeCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

run :: IO ()
run = do
    defaultMain app initialState
    return ()