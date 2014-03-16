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
type Board = [[Maybe Tile]]
data Stage = Play | Animation deriving (Show, Eq)

(!) :: Board -> (Int, Int) -> Maybe Tile
board ! (x, y) = (board !! x) !! y

tileWidth = 100
tileHeight = 100

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
    (InWindow "ones" (500, 500) (1, 1))
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

tileColor 2 = makeColorHex "eee4da"
tileColor 4 = makeColorHex "ede0c8"
tileColor 8 = makeColorHex "f2b179"
tileColor 16 = makeColorHex "f59563"
tileColor 32 = makeColorHex "f67c5f"
tileColor 64 = makeColorHex "f65e3b"
tileColor 128 = makeColorHex "edcf72"
tileColor 256 = makeColorHex "edcc61"
tileColor _ = white

--------------------------------------------------------------------------------
drawBoard :: (Board, Stage) -> IO Picture
drawBoard (board, Play) = return tiles
 where
  tiles = mconcat
    [ translate (fromIntegral $ (x - 2) * tileWidth)
                (fromIntegral $ (y - 2) * tileHeight) $ 
        (color (tileColor tile) $ box tileWidth tileHeight) <>
          (scale (0.5) (0.5) $ color black $ translate (50.0) (50.0) $ text (show tile))
    | x <- [0..3]
    , y <- [0..3]
    , Just tile <- [board ! (x, y)]
    ]

double Nothing = Nothing
double (Just x) = Just $ x*2

handleInput :: Event -> (Board, Stage) -> IO (Board, Stage)
handleInput (EventKey (Char 'a') Up _ (x, y)) (board, Play) = do
    newBoard <- addRandomTile board
    return $ (newBoard, Play)

handleInput (EventKey (SpecialKey KeyLeft) Up _ (x, y)) (board, Play) = do
    let newBoard = foldl moveLeft board [(x, y) | x <- [1..3], y <- [0..3], Just tile <- [board ! (x, y)]]
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput (EventKey (SpecialKey KeyRight) Up _ (x, y)) (board, Play) = do
    let newBoard = foldl moveRight board [(x, y) | x <- [2, 1, 0], y <- [0..3], Just tile <- [board ! (x, y)]]
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput (EventKey (SpecialKey KeyUp) Up _ (x, y)) (board, Play) = do
    let newBoard = foldl moveUp board [(x, y) | x <- [0..3], y <- [2, 1, 0], Just tile <- [board ! (x, y)]]
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput (EventKey (SpecialKey KeyDown) Up _ (x, y)) (board, Play) = do
    let newBoard = foldl moveDown board [(x, y) | x <- [0..3], y <- [1..3], Just tile <- [board ! (x, y)]]
    newBoard2 <- addRandomTile newBoard
    return (newBoard2, Play)

handleInput _ board = return board

for = flip map

moveLeft :: Board -> (Int, Int) -> Board
moveLeft board (x, y) = move board (x, y) (newX, y)
  where newX      = fromMaybe 0 $ find validX [x, x-1 .. 0]
        validX x_ = (x_ /= x && board ! (x_, y) == board ! (x, y)) ||
                      (x_ > 0 && (board ! (x_-1, y) /= board ! (x, y)) && (isJust $ board ! (x_-1, y)))

moveRight :: Board -> (Int, Int) -> Board
moveRight board (x, y) = move board (x, y) (newX, y)
  where newX      = fromMaybe 3 $ find validX [x, x+1 .. 3]
        validX x_ = (x_ /= x && board ! (x_, y) == board ! (x, y)) ||
                      (x_ < 3 && (board ! (x_+1, y) /= board ! (x, y)) && (isJust $ board ! (x_+1, y)))

moveUp :: Board -> (Int, Int) -> Board
moveUp board (x, y) = move board (x, y) (x, newY)
  where newY      = fromMaybe 3 $ find validY [y, y+1 .. 3]
        validY y_ = (y_ /= y && board ! (x, y_) == board ! (x, y)) ||
                      (y_ < 3 && (board ! (x, y_+1) /= board ! (x, y)) && (isJust $ board ! (x, y_+1)))

moveDown :: Board -> (Int, Int) -> Board
moveDown board (x, y) = move board (x, y) (x, newY)
  where newY      = fromMaybe 0 $ find validY [y, y-1 .. 0]
        validY y_ = (y_ /= y && board ! (x, y_) == board ! (x, y)) ||
                      (y_ > 0 && (board ! (x, y_-1) /= board ! (x, y)) && (isJust $ board ! (x, y_-1)))

move board (x, y) (newX, newY)
  -- we aren't going anywhere
  | newX == x && newY == y = board
  -- they are both the same, combine
  | (board ! (newX, newY)) == (board ! (x, y)) = set x y Nothing $ set newX newY (double $ board ! (newX, newY)) board
  | otherwise = set x y Nothing $ set newX (newY) (board ! (x, y)) board

set x y val board = ix x . ix y .~ val $ board
--------------------------------------------------------------------------------
stepGame :: Float -> (Board, Stage) -> IO (Board, Stage)
stepGame _ state = return state
