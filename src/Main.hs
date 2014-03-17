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
              _value    :: Maybe Int,
              _tileX    :: Int,
              _tileY    :: Int,
              _newValue :: Maybe Int,
              _newX     :: Int,
              _newY     :: Int,
              _scaleF    :: Float

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

tileWidth  = 100
tileHeight = 100
space      = 10
boardWidth = tileWidth * 4 + space * 5

initialBoard :: Board
initialBoard = setPositions $ replicate 4 $ replicate 4 (Tile Nothing 0 0 Nothing 0 0 1.0)

setPositions :: Board -> Board
setPositions board = setPositionsWith id id board
    
setPositionsWith :: (Int -> Int) -> (Int -> Int) -> Board -> Board
setPositionsWith fx fy board = 
    for [0..3] $ \x ->
        for [0..3] $ \y -> setPosition_ x y
  where setPosition_ x y = (board ! (x, y)) { _newX = (calcX x), _newY = (calcY y) }
        calcX x = (fx ((x - 2) * (tileWidth + space)  + (space // 2)))
        calcY y = (fy ((y - 2) * (tileHeight + space) + (space // 2)))

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
  let plays = [ set (ix x . ix y . value) (Just 2) $
                set (ix x . ix y . newValue) (Just 2) $
                set (ix x . ix y . tileX) (tile ^. newX) $
                set (ix x . ix y . tileY) (tile ^. newY) $ board
              | x <- [0..3]
              , y <- [0..3]
              , tile@(Tile Nothing _ _ Nothing _ _ _) <- [board ! (x, y)]
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
drawBoard board = return $ grid <> tiles
 where
  grid = mconcat
    [ translate (fromIntegral $ (x - 2) * (tileWidth + space)  + (space // 2))
                (fromIntegral $ (y - 2) * (tileHeight + space) + (space // 2)) $
                (color (makeColor8 204 192 179 255) $ box tileWidth tileHeight)
    | x <- [0..3]
    , y <- [0..3]
    ]
  tiles = mconcat
    [ translate (fromIntegral $ (tile ^. tileX) - ((boxWidth - tileWidth) // 2))
                (fromIntegral $ (tile ^. tileY) - ((boxHeight - tileHeight) // 2)) $
        (color (bgColor tile) $ box boxWidth boxHeight) <>
          (scale (0.5) (0.5) $ translate (75.0) (50.0) $ textFor tile)
    | x <- [0..3]
    , y <- [0..3]
    , tile <- [board ! (x, y)]
    , isJust (tile ^. value)
    , s <- [tile ^. scaleF]
    , boxWidth <- [(floor $ (fromIntegral tileWidth) * s)]
    , boxHeight <- [(floor $ (fromIntegral tileHeight) * s)]
    ]

on (EventKey (SpecialKey KeyLeft) Down _ _) board = do
    addRandomTile . setPositions . transpose . map shiftRow . transpose $ board

on (EventKey (SpecialKey KeyRight) Down _ _) board = do
    addRandomTile . setPositions . transpose . map (reverse . shiftRow . reverse) . transpose $ board

on (EventKey (SpecialKey KeyUp) Down _ _) board = do
    addRandomTile . setPositions . map (reverse . shiftRow . reverse) $ board

on (EventKey (SpecialKey KeyDown) Down _ _) board = do
    addRandomTile . setPositions . map shiftRow $ board

on _ board = return board

for = flip map
groupOn func = groupBy $ \a b -> func a == func b

shiftRow :: Row -> Row
shiftRow = take 4 . (++ repeat (Tile Nothing 0 0 Nothing 0 0 1.0)) . concatMap f . groupOn _value . filter (isJust . _value)
  where f (x : y : xs) | (x ^. value) == (y ^. value) = ((set scaleF 1.5 $ set newValue ((*2) <$> (x ^. value)) x) : (set newValue Nothing y)  : xs); f r = r

speed = 10

addX tile
    | tile ^. tileX < tile ^. newX = over tileX (+speed) tile
    | tile ^. tileX > tile ^. newX = over tileX (subtract speed) tile
    | otherwise = tile

addY tile
    | tile ^. tileY < tile ^. newY = over tileY (+speed) tile
    | tile ^. tileY > tile ^. newY = over tileY (subtract speed) tile
    | otherwise = tile

checkValue tile
    | (tile ^. tileX == tile ^. newX) && (tile ^. tileY == tile ^. newY) = 
        if (isNothing $ tile ^. newValue)
          then set value Nothing tile
          -- else if (tile ^. newValue /= tile ^. value)
          --       then set scaleF (1.5) tile
          else tile
    | otherwise = tile

checkScale tile
    | tile ^. scaleF <= 1.0 = set value (tile ^. newValue) tile
    | otherwise = over scaleF (subtract 0.1) tile

--------------------------------------------------------------------------------
stepGame :: Float -> Board -> IO Board
stepGame _ board = do
    let newBoard =
          for [0..3] $ \x ->
            for [0..3] $ \y -> checkScale . checkValue . addY . addX $ (board ! (x, y))
    return newBoard
