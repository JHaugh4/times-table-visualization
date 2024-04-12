module GlossMain where

import Data.Map.Strict (Map)

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import qualified TimesTable as TT

import Data.List

{-
Gloss model (state) which holds the all the colors,
the current times table number being shown and a map
from a numbered point around the circle to its coordinates.
-}
type Model = ([Color], Float, Map Int (Float, Float))

{-
A fun little function to produce n values between
the two given colors.
-}
colorsBetween :: Int -> Color -> Color -> [Color]
colorsBetween n c1 c2 = unfoldr go (rgba1, n)
    where
        rgba1@(r1, g1, b1, a1) = rgbaOfColor c1
        (r2, g2, b2, a2) = rgbaOfColor c2
        diffs@[rdiff, gdiff, bdiff, adiff] = [r1 - r2, g1 - g2, b1 - b2, a1 - a2]
        [rstep, gstep, bstep, astep]       = map (\diff -> diff / fromIntegral n) diffs
        go (_, 0) = Nothing
        go ((r, g, b, a), m) = Just (makeColor r g b a, ((r - rstep, g - gstep, b - bstep, a - astep), m - 1))

myColors :: [Color]
myColors = colorsBetween 150 c1 c2
    where
        a  = 255
        c1 = makeColorI 180 30 30 a
        c2 = makeColorI 30 180 180 a

{-
Initial state of the model, note that the list of colors
is an infinite list that goes from the beginning to the end
to the beginning and then cycles.
-}
initialModel :: Float -> Int -> Float -> Model
initialModel ttn num radius =
    (cycle (myColors ++ reverse myColors), ttn, TT.genPoints num radius)

{-
Draw the lines produces from the pure code using the Line
picture constructor.
-}
drawLines :: Model -> Picture
drawLines (color:colors, ttn, mp) = Pictures (map (Color color . Line) lines)
    where
        lines = TT.genLines ttn mp

{-
Update the model between frames by taking a color off the list,
adding the step to the times table number and keeping the map
unchanged.
-}
updateModel :: Float -> Model -> Model
updateModel stepSize (colors, ttn, mp) = (tail colors, ttn + stepSize, mp)

-- Put it all together with values given from Brick.
run :: (Int, Int, Float, Int, Float) -> IO ()
run (ws, fps, ttn, poc, ss) = do
    let window = InWindow "Times Table Visualization" (ws, ws) (0, 0)
    simulate window black fps (initialModel ttn poc (fromIntegral ws / 2)) drawLines (\vp frame -> updateModel ss)