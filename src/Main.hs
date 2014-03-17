{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, putMVar, newEmptyMVar)
import Control.Lens (ix, (.~), over, makeLenses, (^.), set)
import Control.Monad
import Data.Monoid ((<>), mconcat, mempty)
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Maybe
import qualified Debug.Trace as D
import Graphics.Rendering.OpenGL (multisample, Capability(..))
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.UI.GLUT (initialDisplayMode, DisplayMode(..))

data Tile = Tile {
              _value :: Maybe Int,
              _tileX :: Int,
              _tileY :: Int
              } deriving (Show, Eq)

makeLenses ''Tile

type Row   = [Tile]
type Board = [Row]

(//) :: Int -> Int -> Int
a // b = floor $ (fromIntegral a) / (fromIntegral b)

(***) :: Int -> Int -> Int
a *** b = floor $ (fromIntegral a) ** (fromIntegral b)

(!) :: Board -> (Int, Int) -> Tile
board ! (x, y) = (board !! x) !! y

tileWidth  = 107
tileHeight = 107
space      = 14
boardWidth = tileWidth * 4 + space * 5

initialBoard :: Board
initialBoard = setPositions $ replicate 4 $ replicate 4 (Tile Nothing 0 0)

setPositions :: Board -> Board
setPositions board = for [0..3] $ \x ->
                        for [0..3] $ \y -> set tileX ((x - 2) * (tileWidth + space)  + (space // 2)) $
                                           set tileY ((y - 2) * (tileHeight + space) + (space // 2)) (board ! (x, y))
convert :: Char -> Int
convert 'a' = 10
convert 'b' = 11
convert 'c' = 12
convert 'd' = 13
convert 'e' = 14
convert 'f' = 15
convert char = read [char]

hexToInt :: String -> Int
hexToInt (x:[]) = convert x
hexToInt str@(x:xs) = 16 *** (length str - 1) * (convert x) + hexToInt xs

makeColorHex (r1:r2:g1:g2:b1:b2:[]) = makeColor8
                                        (hexToInt [r1, r2])
                                        (hexToInt [g1, g2])
                                        (hexToInt [b1, b2])
                                        255

--------------------------------------------------------------------------------
main :: IO ()
main = do
  initialDisplayMode $~ (Multisampling:) -- supposed to anti-alias, doesn't work very well
  startingBoard <- return initialBoard >>= addRandomTile >>= addRandomTile
  playIO
    (InWindow "ones" (boardWidth, boardWidth) (1, 1))
    (makeColorHex "bbada0")
    30
    startingBoard
    drawBoard
    on
    stepGame

box :: Int -> Int -> Picture
box w_ h_ = polygon [p1, p2, p3, p4]
  where
    w = fromIntegral w_
    h = fromIntegral h_
    p1 = (0, 0)
    p2 = (0, w)
    p3 = (h, w)
    p4 = (h, 0)

addRandomTile :: Board -> IO Board
addRandomTile board = do
  -- Choose a random move
  let plays = [ set (ix x . ix y . value) (Just 2) board
              | x <- [0..3]
              , y <- [0..3]
              , Tile Nothing _ _ <- [board ! (x, y)]
              ]
    
  (plays !!) <$> randomRIO (0, length plays - 1)

bgColor tile
  | isNothing $ tile ^. value          = makeColor8 204 192 179 255
  | (fromJust $ tile ^. value) == 2    = makeColorHex "eee4da"
  | (fromJust $ tile ^. value) == 4    = makeColorHex "ede0c8"
  | (fromJust $ tile ^. value) == 8    = makeColorHex "f2b179"
  | (fromJust $ tile ^. value) == 16   = makeColorHex "f59563" 
  | (fromJust $ tile ^. value) == 32   = makeColorHex "f67c5f" 
  | (fromJust $ tile ^. value) == 64   = makeColorHex "f65e3b" 
  | (fromJust $ tile ^. value) == 128  = makeColorHex "edcf72"
  | (fromJust $ tile ^. value) == 256  = makeColorHex "edcc61"
  | (fromJust $ tile ^. value) == 512  = makeColorHex "edc850"
  | (fromJust $ tile ^. value) == 1024 = makeColorHex "edc53f"
  | (fromJust $ tile ^. value) == 2048 = makeColorHex "edc22e"

fontColor tileValue
  | tileValue == 2 || tileValue == 4 = makeColorHex "776e65"
  | otherwise = makeColorHex "f9f6f2"

textFor tile = case tile ^. value of
                 Nothing -> text ""
                 Just tileValue -> color (fontColor tileValue) $ text . show $ tileValue

--------------------------------------------------------------------------------
drawBoard :: Board -> IO Picture
drawBoard board = return tiles
 where
  tiles = mconcat
    [ translate (fromIntegral $ tile ^. tileX)
                (fromIntegral $ tile ^. tileY) $
        (color (bgColor tile) $ box tileWidth tileHeight) <>
          (scale (0.5) (0.5) $ translate (75.0) (50.0) $ textFor tile)
    | x <- [0..3]
    , y <- [0..3]
    , tile <- [board ! (x, y)]
    ]

on (EventKey (SpecialKey KeyLeft) Down _ _) board = do
    liftM setPositions $ addRandomTile . transpose . map shiftRow . transpose $ board

on (EventKey (SpecialKey KeyRight) Down _ _) board = do
    liftM setPositions $ addRandomTile . transpose . map (reverse . shiftRow . reverse) . transpose $ board

on (EventKey (SpecialKey KeyUp) Down _ _) board = do
    liftM setPositions $ addRandomTile . map (reverse . shiftRow . reverse) $ board

on (EventKey (SpecialKey KeyDown) Down _ _) board = do
    liftM setPositions $ addRandomTile . map shiftRow $ board

on _ board = return board

for = flip map
groupOn func = groupBy $ \a b -> func a == func b

shiftRow :: Row -> Row
shiftRow = take 4 . (++ repeat (Tile Nothing 0 0)) . concatMap f . groupOn _value . filter (isJust . _value)
  where f (x : y : xs) | (x ^. value) == (y ^. value) = ((over value (fmap (*2)) x) : xs); f r = r

--------------------------------------------------------------------------------
stepGame :: Float -> Board -> IO Board
stepGame _ state = return state
