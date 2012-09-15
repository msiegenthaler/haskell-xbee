module Data.Circular (
    Circular(..),
    findFree
) where

-- | Type that consists of a 'circular' space of values.
class Eq c => Circular c where
    -- | The 'zero' value. Since the type is circular it usually does not matter which value is
    -- chosen as 'zero'.
    initial :: c
    -- | The value following the argument. Wraps around.
    next :: c -> c


-- | Finds a free value, one that is not already used inside the list. Best performance if the
-- values in the set are in 'ascending' order.
findFree :: Circular c => [c] -> Maybe c
findFree [] = Just initial
findFree keys = check keys lst (next lst)
    where lst = last keys
          check ks k c | k == c        = Nothing
                       | c `elem` keys = check ks k (next c)
                       | otherwise     = Just c
