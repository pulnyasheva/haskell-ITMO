module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate
  , application
  , takeGrid
  , initialGrid
  ) where

import Control.Comonad (Comonad (..))
import Data.Grid (Grid (..))
import Data.ListZipper (ListZipper (..), shiftLeft, shiftRight)
import Options.Applicative
import System.Random (StdGen, mkStdGen, randomR)

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

instance Show Cell where
  show cell = case cellState cell of
    Healthy    -> "_"
    Infected _ -> "i"
    Ill _      -> "#"
    Immune _   -> "@"

type Comonad19Grid = Grid Cell

initialCellHealthy :: Int -> Cell
initialCellHealthy x = Cell Healthy (mkStdGen x)

initialCellInfected :: Int -> Cell
initialCellInfected x = Cell (Infected 0) (mkStdGen x)

generateCell :: Cell -> Cell
generateCell cell = let (_, newRand) = randomR (0, 1) (cellRand cell) :: (Double, StdGen)
                        state = cellState cell
                    in Cell state newRand

initialList :: Int -> [Cell]
initialList x = iterate generateCell (initialCellHealthy x)

initialListZipperHealthy :: ListZipper Cell
initialListZipperHealthy = LZ (initialList 0) (initialCellHealthy 500) (initialList 1000)

initialListZipperInfected :: ListZipper Cell
initialListZipperInfected = LZ (initialList 0) (initialCellInfected 500) (initialList 1000)

initialGrid :: Comonad19Grid
initialGrid = let left = iterate shiftLeft (shiftLeft initialListZipperHealthy)
                  right = iterate shiftRight (shiftRight initialListZipperHealthy)
              in Grid $ LZ left initialListZipperInfected right

-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).
simulate :: Config -> [Comonad19Grid]
simulate config = iterate (extend (updateCell config)) initialGrid

updateCell :: Config -> Comonad19Grid -> Cell
updateCell config grid =
  let cell = extract grid
      neighbors = getNeighbors grid
      infectedNeighbors = isInfected neighbors
      (randomNumber, newRand) = randomR (0, 1) (cellRand cell)
      newState =
        case cellState cell of
          Infected n ->
            if n == incubationPeriod config
            then Ill 0
            else Infected (n + 1)
          Ill n ->
            if n == illnessDuration config
            then Immune 0
            else Ill (n + 1)
          Immune n ->
            if n == immunityDuration config
            then Healthy
            else Immune (n + 1)
          Healthy ->
            if infectedNeighbors && randomNumber < probability config
              then Infected 0
              else Healthy
   in Cell newState newRand

getNeighbors :: Comonad19Grid -> [Cell]
getNeighbors (Grid (LZ [] v []))           = getLeftAndRight v
getNeighbors (Grid (LZ [] v (r : _)))      = extract r : getLeftAndRight v
getNeighbors (Grid (LZ (l : _) v []))      = extract l : getLeftAndRight v
getNeighbors (Grid (LZ (l : _) v (r : _))) = extract l : extract r : getLeftAndRight v

getLeftAndRight :: ListZipper Cell -> [Cell]
getLeftAndRight (LZ [] _ [])           = []
getLeftAndRight (LZ [] _ (r : _))      = [r]
getLeftAndRight (LZ (l : _) _ [])      = [l]
getLeftAndRight (LZ (l : _) _ (r : _)) = [l, r]

isInfected :: [Cell] -> Bool
isInfected cells = let len = length $ filter (\(Cell state _) -> case state of
                                                                  Infected _ -> True
                                                                  Ill _      -> True
                                                                  _          -> False) cells
                   in (len > 0)

configParser :: Parser Config
configParser = Config
      <$> option auto
         (long "prob"
         <> metavar "PROBABILITY"
         <> help "Infection probability"
         <> showDefault
         <> value 0.5)
      <*> option auto
         (long "incub"
         <> metavar "INCUBATION_PERIOD"
         <> help "Incubation period duration"
         <> showDefault
         <> value 2)
      <*> option auto
         (long "ill"
         <> metavar "ILLNESS_DURATION"
         <> help "Illness duration"
         <> showDefault
         <> value 3)
      <*> option auto
         (long "immun"
         <> metavar "IMMUNITY_DURATION"
         <> help "Immunity duration"
         <> showDefault
         <> value 5)

gridSizeParser :: Parser Int
gridSizeParser = option auto
         (long "grid-size"
         <> metavar "GRID_SIZE"
         <> help "Output grid size"
         <> showDefault
         <> value 11)

iterationsParser :: Parser Int
iterationsParser = option auto
         (long "iterations"
         <> metavar "ITERATIONS"
         <> help "The number of simulation iterations"
         <> showDefault
         <> value 10)

optionsParser :: Parser (Config, Int, Int)
optionsParser = (,,) <$> configParser <*> gridSizeParser <*> iterationsParser

takeListZipper :: Int -> ListZipper a -> ListZipper a
takeListZipper size (LZ left v right) = LZ (take size left) v (take size right)

takeGrid :: Int -> Grid a -> Grid a
takeGrid size gr = let lz = takeListZipper size $ unGrid gr
                   in Grid $ fmap (takeListZipper size) lz

application :: IO ()
application = do
  parameters <- execParser opts
  let checkOpts = checkConfig parameters
  case checkOpts of
    Left er -> putStrLn er
    Right (config, size, iterations) -> let grids = take iterations (simulate config)
                                        in mapM_ (print . takeGrid ((size - 1) `div` 2)) grids
  where
      opts = info
          (optionsParser <**> helper)
          (fullDesc
          <> progDesc "Simulate Covid-19 infection on a 2-dimensional grid"
          <> header "comonad19 - a Covid-19 simulation")


checkConfig :: (Config, Int, Int) -> Either String (Config, Int, Int)
checkConfig opts@(cn, size, iter)
  | 0 > probability cn || probability cn > 1 = Left "Incorrect probability, probability lies in the range from 0 to 1"
  | incubationPeriod cn <= 0 = Left "The incubation period is incorrect, it should greater than 0"
  | illnessDuration cn <= 0 = Left "The illness duration is incorrect, it should greater than 0"
  | immunityDuration cn <= 0 = Left "The immunity duration is incorrect, it should greater than 0"
  | size <= 0 || even size = Left "The size is incorrect, it must be odd and greater than 0"
  | size <= 0 || even size = Left "The size is incorrect, it must be odd and greater than 0"
  | iter <= 0 = Left "The iterations is incorrect, it should greater than 0"
  | otherwise = Right opts
