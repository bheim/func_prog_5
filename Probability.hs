module Probability where

import qualified Data.Map as Map
import Data.Array (Array, array, (!))


{-
To demonstrate requested functionalities,
ghci -package containers -package array Probability.hs

Part 2 -> normalize twoRolls
Part 3 ->
    import Data.List (maximumBy)
    import Data.Ord (comparing)
    maximumBy (comparing snd) $ getProbabilities (rollDice 100)

Part 4 -> firstPlayerWins rollingStones
- not nicely formatted but shows results

-}

newtype Probability a
  = Probability {getProbabilities :: [(a, Double)]}
  deriving (Show)

roll :: Probability Int
-- simple roll Probability
roll = Probability [(i, 1 / 6) | i <- [1 .. 6]]

instance Functor Probability where
 -- basically, apply the function to the value but not the probability
    fmap f (Probability ps) = Probability [(f a, p) | (a, p) <- ps]

instance Applicative Probability where
-- the key here is that we multiply probabilities of independent events against each other, which will be really helpful later
    pure p = Probability [(p, 1)]
    (Probability fs) <*> (Probability as) = Probability [(f a, fp * ap ) | (f, fp) <- fs, (a, ap) <- as]

instance Monad Probability where
    (Probability ps) >>= f = Probability [(aa, p * pp) | (a, p) <- ps, (aa, pp) <- getProbabilities(f a)]

twoRolls :: Probability Int
twoRolls = do
    a <- roll
    b <- roll
    -- now we add and wrap it
    pure $ a + b

normalize :: (Ord a) => Probability a -> Probability a
-- Map.fromListWith condenses it and then Map.toAscList will then sort it
normalize (Probability ps) = Probability ((Map.toAscList . Map.fromListWith (+)) ps)

rollDice :: Int -> Probability Int
-- recursive call to add next dice roll
rollDice 0 = pure 0
rollDice n = normalize $ do
    distribution <- rollDice (n - 1)
    r <- roll
    pure $ distribution + r

rollingStones :: Array Int (Probability Int)
-- uses algorithm prescribed by Kurtz. Rolls, checks remaining, if there's more, check the values at the remaining value
-- smartly builds from ground up
rollingStones = array (0, 50) [(n, f n) | n <- [0..50]]
  where
    f 0 = pure 1
    f n = normalize $ do
        r <- roll
        let remaining = max 0 (n - r)
        if remaining == 0
            then pure 1
            else do
                rest <- rollingStones ! remaining
                pure (rest + 1)

firstPlayerWins :: Array Int (Probability Int) -> [(Int, Double)]
-- this adds up all the win probabilities from odd rolls which indicates player 1 wins.
firstPlayerWins rs = [(n, sum[p | (k, p) <- getProbabilities (rs ! n), odd k]) | n <- [2..50]]