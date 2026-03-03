module Probability where

newtype Probability a
  = Probability {getProbabilities :: [(a, Double)]}
  deriving (Show)

roll :: Probability Int
roll = Probability [(i, 1 / 6) | i <- [1 .. 6]]

instance Functor Probability where
    fmap f (Probability ps) = Probability = [(f a, p) | (a, p) <- ps]

instance Applicative Probability where
    pure p = Probability [(p, 1)]
    (Probability fs) <*> (Probability as) = Probability [(f a, fp * ap ) | (f, fp) <- fs, (a, ap) <- as]

instance Monad Probability where
    (Probability ps) >>= f = Probability [(aa, p * pp) | (a, p) <- ps, (aa, pp) <- getProbabilities(f a)]

twoRolls :: Probability Int
twoRolls = do
    a <- roll
    b <- roll
    pure $ a + b

normalize :: (Ord a) => Probability a -> Probability a
normalize (Probability ps) = Probability ((Map.toAscList . Map.fromListWith (+)) ps)

rollDice :: Int -> Probability Int
rollDice 0 = pure 0
rollDice n = normalize $ do
    distribution <- rollDice (n - 1)
    r <- roll
    pure $ distribution + r

rollingStones :: Array Int (Probability Int)
rollingStones = array (0, 50) [(n, f n) | n <- [0..50]]
where
    f 0 = 1
    f n = normalize $ do
        r <- roll
        let remaining = max 0 (n - r)
        if remaining == 0:
            then pure 1
        else do
            rest <- rollingStones ! reamining
            pure(rest + 1)

rollingStonesData :: Array Int (Probability Int)

