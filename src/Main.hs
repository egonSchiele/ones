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

type Tile = Int
type Board = [[Maybe Tile]]
data Stage = Play | Animation deriving (Show, Eq)

tileWidth = 100
tileHeight = 100

initialBoard :: Board
initialBoard = replicate 4 (replicate 4 Nothing)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  playIO
    (InWindow "ones" (500, 500) (1, 1))
    azure
    5
    (initialBoard, Play)
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
              , Nothing <- [ (board !! x) !! y ]
              ]
    
  newBoard <- (plays !!) <$> randomRIO (0, length plays - 1)
  return newBoard

--------------------------------------------------------------------------------
drawBoard :: (Board, Stage) -> IO Picture
drawBoard (board, Play) = return tiles
 where
  tiles = mconcat
    [ translate (fromIntegral $ (x - 2) * tileWidth)
                (fromIntegral $ (y - 2) * tileHeight) $
        (color white $ box tileWidth tileHeight) <>
          (scale (0.5) (0.5) $ color black $ translate (50.0) (50.0) $ text (show tile))
    | x <- [0..3]
    , y <- [0..3]
    , Just tile <- [ (board !! x) !! y ]
    ]

handleInput :: Event -> (Board, Stage) -> IO (Board, Stage)
handleInput (EventKey (Char 'a') Up _ (x, y)) (board, Play) = do
    newBoard <- addRandomTile board
    return $ (newBoard, Play)

handleInput (EventKey (SpecialKey KeyLeft) Up _ (x, y)) (board, Play) = return (newBoard, Play)
  where
    newBoard = foldl moveLeft board [(x, y) | x <- [0..3], y <- [0..3], Just tile <- [ (board !! x) !! y ]]
    moveLeft board (0, _) = board
    moveLeft board (x, y) = case find (\x_ -> isJust $ (board !! x_) !! y) [x-1, x-2 .. 0] of
                              Nothing -> set x y Nothing $ set 0 y ((board !! x) !! y) board
                              Just takenX -> if takenX == x - 1
                                               then board
                                               else set x y Nothing $ set (takenX + 1) y ((board !! x) !! y) board

handleInput _ board = return board

set x y val board = ix x . ix y .~ val $ board
--------------------------------------------------------------------------------
stepGame :: Float -> (Board, Stage) -> IO (Board, Stage)
stepGame _ state = return state
