{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sortBy, zip)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 1: Install MonadRandom
-- With stack: "$ stack install MonadRandom"
-- evalRandIO die  -- output: DV {unDV = 4}


-- Exercise 2: Simulate the battle
rollDice :: Int -> Rand StdGen [DieValue]
rollDice x = replicateM x die

gatherArmies :: Battlefield -> (Army, Army)
gatherArmies bfield = (att, def)
  where
    -- The attacking player may attack with up to three units at a time.
    -- However, they must always leave at least one unit behind.
    att = if attackers bfield > 3 then 3 else attackers bfield - 1
    -- The defending player may defend with up to two units (or only
    -- one if that is all they have).
    def = if defenders bfield > 2 then 2 else defenders bfield

battle :: Battlefield -> Rand StdGen Battlefield
battle bfield =
  let
    (noAttackers, noDefenders) = gatherArmies bfield
  in
    sortBy (flip compare) <$> rollDice noAttackers
    >>= \resultsAttackers ->

    sortBy (flip compare) <$> rollDice noDefenders
    >>= \resultsDefenders ->
         -- resultsXYZ :: [DieValue]
         let
           outcome (a, d) (casA, casD) = if a > d
             then (casA, casD + 1)
             else (casA + 1, casD)
           battles = zip resultsAttackers resultsDefenders
           (casualtiesA, casualtiesD) = foldr outcome (0, 0) battles
         in
           return Battlefield { attackers = noAttackers - casualtiesA
                              , defenders = noDefenders - casualtiesD
                              }


-- Exercise 3: Simulate an entire invasion attempt
invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | attackers b < 2 || defenders b == 0 = return b
  | otherwise = battle b >>= invade


-- Exercise 4: Run invade 1000 times, and computes the estimated probability
--   that the attacking army will defeat the defending army.
successProb :: Battlefield -> Rand StdGen Double
successProb bfield =
  replicateM 1000 (invade bfield) >>= wonInvasions >>= probability
  where
    battleToPoints (Battlefield a d) = if a > d then 1 else 0
    wonInvasions = return . sum . map battleToPoints
    probability success = return $ success / 1000


-- Exercise 5: Computes the exact probability of success using formulas.
exactSuccessProb :: Battlefield -> Double
exactSuccessProb bfield
  | def < 1  = 1
  | att <= 1 = 0  -- Since it needs to leave a unit behind he can't attack
  | otherwise = sum (exactSuccessProb <$> possibilities) / 3
  -- P(att = 3 & def = 2) + P(att = 2 & def = 2) +
  -- P(att = 3 & def = 1) + P(att = 2 & def = 1) / 4
  where
    att = attackers bfield
    def = defenders bfield
    -- Current exactSuccessProb is 1 of 4, take only other 3 of 4 possibilities
    possibilities = [ Battlefield { attackers = att, defenders = def - 2 }
                    , Battlefield { attackers = att - 2, defenders = def }
                    , Battlefield { attackers = att - 1, defenders = def -1 }
                    ]


instance Show Battlefield where
  show b = att ++ " attackers and " ++ def ++ " defenders"
    where att = show $ attackers b
          def = show $ attackers b


main :: IO ()
main = do
  let battlefield = Battlefield { attackers = 3, defenders = 2 }
      exactProbability = exactSuccessProb battlefield

  estProb <- evalRandIO $ successProb battlefield
  putStrLn $ "Attacking with " ++ show battlefield

  putStrLn $ "  has an estimated probability of: " ++ show estProb
  putStrLn $ "  and an \"exact\" probability of: " ++ show exactProbability
