
-- For forall a
{-# LANGUAGE ExistentialQuantification #-}
-- For data Compare a where
{-# LANGUAGE GADTs                     #-}

data T = forall a. MkT a

mkT :: a -> T
mkT = MkT

a =  [mkT "a", mkT 1, mkT 'c']

data T2 = forall a. MkT2 (a, a -> Int)

mkT2 :: (a, a -> Int) -> T2
mkT2 = MkT2

a2 = [mkT2 (2 :: Int, (* 2)), mkT2 (4 :: Int, (+1))]

data Obj = forall a. (Show a) => Obj a

xs :: [Obj]
xs = [Obj 1, Obj "foo", Obj 'c']

doShow :: [Obj] -> String
doShow [] = ""
doShow (Obj x : xs) = show x ++ doShow xs

data Compare a where
  ASC  :: Ord b => (a -> b) -> Compare a
  DESC :: Ord b => (a -> b) -> Compare a



