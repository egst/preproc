module Tools where
{- Misc. tools -}

import Data.Maybe (fromMaybe)

-- Head or alternative for empty lists:

headOr :: t -> [t] -> t
headOr x [] = x
headOr _ l = head l

-- Map on list of tuples or lists:
map2 :: (t -> t', u -> u')          -> [(t, u)]    -> [(t', u')]
map3 :: (t -> t', u -> u', v -> v') -> [(t, u, v)] -> [(t', u', v')]
mapl :: (t -> u) -> [[t]] -> [[u]]
map2 (f1, f2)     l = [(f1 a, f2 b)       | (a, b)    <- l]
map3 (f1, f2, f3) l = [(f1 a, f2 b, f3 c) | (a, b, c) <- l]
mapl f = map $ map f

-- Operation on first or first two elements of a list:
onHead  :: (t -> t)      -> [t] -> [t]
onHead2 :: (t -> t -> t) -> [t] -> [t]
onHead f l = case l of
    [] -> l
    first : rest -> f first : rest
onHead2 f l = case l of
    []  -> l
    [_] -> l
    first : second : rest -> f second first : rest

-- Binary function composition:
infixr 9 .:
infixr 9 .:.
(.:.) :: (v -> v -> t) -> (u -> v) -> (u -> u -> t) -- Binary with unary
(.:)  :: (w -> t) -> (u -> v -> w) -> (u -> v -> t) -- Unary with binary
(f .:. g) x y = f (g x) (g y)
(.:) = (.).(.)

-- Single value list (used in point-free function definitions):
singleton :: t -> [t]
singleton x = [x]

-- Infix elem shorthand:
infixl 4 <<
infixl 4 </
-- TODO: For any Foldable.
--(<<) :: (Eq t, Foldable f) => t -> f t -> Bool
--(</) :: (Eq t, Foldable f) => t -> f t -> Bool
(<<) :: Eq t => t -> [t] -> Bool
(</) :: Eq t => t -> [t] -> Bool
(<<) = elem
(</) = (not .) . (<<)

-- Used for debugging:

data FancyTuple2 t u     = FT2 t u
data FancyTuple3 t u v   = FT3 t u v
data FancyTuple4 t u v w = FT4 t u v w

instance (Show t, Show u) => Show (FancyTuple2 t u) where
    show (FT2 x y) = show x ++ "\n" ++ show y
instance (Show t, Show u, Show v) => Show (FancyTuple3 t u v) where
    show (FT3 x y z) = show x ++ "\n" ++ show y ++ "\n" ++ show z
instance (Show t, Show u, Show v, Show w) => Show (FancyTuple4 t u v w) where
    show (FT4 x y z w) = show x ++ "\n" ++ show y ++ "\n" ++ show z ++ "\n" ++ show w

get2 (f1, f2)         x = FT2 (f1 x) (f2 x)
get3 (f1, f2, f3)     x = FT3 (f1 x) (f2 x) (f3 x)
get4 (f1, f2, f3, f4) x = FT4 (f1 x) (f2 x) (f3 x) (f4 x)