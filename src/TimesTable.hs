module TimesTable where

import Data.List

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.Fixed (mod')

import qualified Debug.Trace as DT

-- Source: https://www.youtube.com/watch?v=qhbuKbxJsk8

{-
Generate n points around a circle. This
is done by dividing up the 2pi angles
into n pieces. Then determining the x and y
coordinates of each point using the following
formula:

x = radius * (cos theta)
y = radius * (sin theta)
-}
genPoints :: Int -> Float -> Map Int (Float, Float)
genPoints num radius = Map.fromList (map toCoord thetas)
    where
        twoPi = 2 * pi
        thetaSep = twoPi / fromIntegral num
        thetas = unfoldr go (0, pi)
        go (n, theta)
            | n == num  = Nothing
            | otherwise = Just ((n, theta), (n + 1, theta - thetaSep))
        toCoord (n, theta) =
            (n, (radius * cos theta, radius * sin theta))

{-
Generate a list of list of points for each entry in the map.
This is done by taking the number from the map (Int) and multiplying
it by the times table number then modding by the size of the map (number
of points around the circle) and seeing where it ends up. We then draw
a line connecting that point with the point where it ended up.
-}
genLines :: Float -> Map Int (Float, Float) -> [[(Float, Float)]]
genLines ttn mp = Map.foldlWithKey go [] mp
    where
        go acc num from =
            let toNum = round (fromIntegral num * ttn) `mod` Map.size mp
                to = mp ! toNum
            in [from, to] : acc