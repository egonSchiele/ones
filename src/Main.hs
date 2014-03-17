import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, putMVar, newEmptyMVar)
import Control.Lens (ix, (.~))
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

type Tile = Int
type Row = [Maybe Tile]
type Board = [Row]
data Stage = Play | Animation deriving (Show, Eq)

(//) :: Int -> Int -> Int
a // b = floor $ (fromIntegral a) / (fromIntegral b)

(!) :: Board -> (Int, Int) -> Maybe Tile
board ! (x, y) = (board !! x) !! y

tileWidth = 107
tileHeight = 107

space = 14

boardWidth = tileWidth * 4 + space * 5

initialBoard :: Board
initialBoard = replicate 4 (replicate 4 Nothing)

(***) :: Int -> Int -> Int
a *** b = floor $ (fromIntegral a) ** (fromIntegral b)

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

makeColorHex (r1:r2:g1:g2:b1:b2:[]) = makeColor8 (hexToInt [r1, r2]) (hexToInt [g1, g2]) (hexToInt [b1, b2]) 255

--------------------------------------------------------------------------------
main :: IO ()
main = do
  initialDisplayMode $~ (Multisampling:) -- supposed to anti-alias, doesn't work very well
  startingBoard <- return initialBoard >>= addRandomTile >>= addRandomTile
  playIO
    (InWindow "ones" (boardWidth, boardWidth) (1, 1))
    (makeColorHex "bbada0")
    30
    (startingBoard, Play)
    drawBoard
    handleInput
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
  let plays = [ (ix x . ix y .~ Just 2) board
              | x <- [0..3]
              , y <- [0..3]
              , Nothing <- [board ! (x, y)]
              ]
    
  newBoard <- (plays !!) <$> randomRIO (0, length plays - 1)
  return newBoard

bgColor Nothing     = makeColor8 204 192 179 255
bgColor (Just 2)    = makeColorHex "eee4da"
bgColor (Just 4)    = makeColorHex "ede0c8"
bgColor (Just 8)    = makeColorHex "f2b179"
bgColor (Just 16)   = makeColorHex "f59563"
bgColor (Just 32)   = makeColorHex "f67c5f"
bgColor (Just 64)   = makeColorHex "f65e3b"
bgColor (Just 128)  = makeColorHex "edcf72"
bgColor (Just 256)  = makeColorHex "edcc61"
bgColor (Just 512)  = makeColorHex "edc850"
bgColor (Just 1024) = makeColorHex "edc53f"
bgColor (Just 2048) = makeColorHex "edc22e"
bgColor _           = white

fontColor (Just x)
  | x == 2 || x == 4 = makeColorHex "776e65"
  | otherwise        = makeColorHex "f9f6f2"

textFor Nothing = text ""
textFor caption@(Just x) = color (fontColor caption) $ text (show x)

--------------------------------------------------------------------------------
drawBoard :: (Board, Stage) -> IO Picture
drawBoard (board, Play) = return tiles
 where
  tiles = mconcat
    [ translate (fromIntegral $ (x - 2) * (tileWidth + space)  + (space // 2))
                (fromIntegral $ (y - 2) * (tileHeight + space) + (space // 2)) $ 
        (color (bgColor tile) $ box tileWidth tileHeight) <>
          (scale (0.5) (0.5) $ translate (75.0) (50.0) $ textFor tile)
    | x <- [0..3]
    , y <- [0..3]
    , tile <- [board ! (x, y)]
    ]

double Nothing = Nothing
double (Just x) = Just $ x*2

handleInput :: Event -> (Board, Stage) -> IO (Board, Stage)
handleInput (EventKey (Char 'a') Up _ (x, y)) (board, Play) = do
    newBoard <- addRandomTile board
    return $ (newBoard, Play)

handleInput (EventKey (SpecialKey KeyLeft) Up _ (x, y)) (board, Play) = do
    let newBoard = transpose . map shiftRow . transpose $ board
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput (EventKey (SpecialKey KeyRight) Up _ (x, y)) (board, Play) = do
    let newBoard = transpose . map (reverse . shiftRow . reverse) . transpose $ board
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput (EventKey (SpecialKey KeyUp) Up _ (x, y)) (board, Play) = do
    let newBoard = map (reverse . shiftRow . reverse) board
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput (EventKey (SpecialKey KeyDown) Up _ (x, y)) (board, Play) = do
    let newBoard = map shiftRow board
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput _ board = return board

for = flip map

shiftRow :: Row -> Row
shiftRow = take 4 . (++ repeat Nothing) . concatMap f . group . filter isJust
  where f (x : y : xs) | x == y = (fmap (*2) x : xs); f r = r

set x y val board = ix x . ix y .~ val $ board
--------------------------------------------------------------------------------
stepGame :: Float -> (Board, Stage) -> IO (Board, Stage)
stepGame _ state = return state
