import Data.Map (Map)
import Data.Random.Source.Std
import qualified Data.Map as Map
import System.Environment
import Data.Random

-- Type definitions for tracks
data TrackTrigger = X | Y | Z | Distance1 | Distance2 deriving (Eq, Show, Enum, Read)
data Track = Track {start :: Int, events :: Map (Int) (TrackTrigger)} deriving (Show, Read)
getAllTracks = [
  Track {start = 15, events = Map.fromList [(4,Distance1),(9,Distance2),(0,Z),(4,Y),(7,Y),(11,X)]},
  Track {start = 10, events = Map.fromList [(0,Z),(7,X),(4,Distance1),(9,Distance2)]},
  Track {start = 14, events = Map.fromList [(0,Z),(2,Y),(6,Y),(9,X),(4,Distance1),(9,Distance2)]},
  Track {start = 11, events = Map.fromList [(0,Z),(2,Y),(7,X),(4,Distance1),(9,Distance2)]},
  Track {start = 9, events = Map.fromList [(0,Z),(4,X),(4,Distance1),(9,Distance2)]},
  Track {start = 13, events = Map.fromList [(0,Z),(6,Y),(10,X),(4,Distance1),(9,Distance2)]},
  Track {start = 12, events = Map.fromList [(0,Z),(4,Y),(8,X),(4,Distance1),(9,Distance2)]}]
getTracks xs = filter (\(track, i) -> elem i xs) (zip getAllTracks [1..])

-- Type definitions for positioning
data ZoneColor = Red | White | Blue deriving (Eq, Show, Enum, Read)
data Floor = Top | Bottom deriving (Eq, Show, Enum, Read)

-- Type definitions for cards
data Action = A | B | C | Bbots | HeroicA | HeroicB | HeroicBbots deriving (Eq, Show, Enum, Read)
data Move = BlueShift | RedShift | FloorChange | Heroic (ZoneColor) (Floor) deriving (Eq, Show, Read)
data Card = Card {action :: Action, move :: Move} deriving (Eq, Show, Read)
getCards = 
    replicate 10 Card {action = A, move = BlueShift} ++
    replicate 8 Card {action = B, move = BlueShift} ++
    replicate 7 Card {action = C, move = BlueShift} ++
    replicate 5 Card {action = Bbots, move = BlueShift} ++
    replicate 10 Card {action = A, move = RedShift} ++
    replicate 8 Card {action = B, move = RedShift} ++
    replicate 7 Card {action = C, move = RedShift} ++
    replicate 5 Card {action = Bbots, move = RedShift} ++
    replicate 10 Card {action = A, move = FloorChange} ++
    replicate 8 Card {action = B, move = FloorChange} ++
    replicate 7 Card {action = C, move = FloorChange} ++
    replicate 5 Card {action = Bbots, move = FloorChange} ++
    [
      Card {action = HeroicA, move = Heroic Red Bottom},
      Card {action = HeroicA, move = Heroic Blue Bottom},
      Card {action = HeroicB, move = Heroic Red Top},
      Card {action = HeroicB, move = Heroic Blue Top},
      Card {action = HeroicBbots, move = Heroic White Top},
      Card {action = HeroicBbots, move = Heroic White Bottom}
    ]

-- Type definitions for ship
data EnergyContainer = EnergyContainer {capacity :: Int, value :: Int} deriving (Eq, Read, Show)
data Gun = Gun {range :: Int, power :: Int, zonesToHit :: [ZoneColor], cost :: Int} deriving (Eq, Read, Show)
data Damage = Damage {description :: String, damgeFn :: (Zone -> Zone)}
data Zone = Zone {
  shield :: EnergyContainer, 
  reactor :: EnergyContainer, 
  topGun :: Gun, 
  lowGun :: Gun, 
  damage :: Int,
  damageTiles :: [Damage]}
data Ship = Ship {
  zones :: [Zone]}

-- Type definitions for players
data Player = Player {
  actions :: [Action],
  playerZone :: Zone}

-- Type definitions for threats
data ThreatColor = WhiteThreat | YellowThreat deriving (Eq, Show, Read, Enum)
data Threat = Threat {
  name :: String,
  color :: ThreatColor,
  isSerious :: Bool,
  health :: Int,
  speed :: Int,
  zone :: ZoneColor,
  floor :: Floor,
  trackPosition :: Int}

-- Type definition for the game
data Game = Game {
  ship :: Ship,
  players :: [Player],
  internalThreats :: [Threat],
  externalThreats :: [Threat],
  internalTrack :: Track}


main = do
  putStrLn $ concat $ map (\card -> (show card) ++ "\n") getCards
