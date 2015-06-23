-- Working example of http://chrisdone.com/posts/existentials

{-# LANGUAGE ExistentialQuantification #-}

data PyShow = forall a. (Show a) => PyShow a
data PyShowNum = forall a. (Show a,Num a) => PyShowNum a

instance Show PyShow where show (PyShow s) = show s
instance Show PyShowNum where show (PyShowNum s) = show s
instance Show (a -> b) where show _ = "<function>"

allShow = [PyShow 1, PyShow 'a', PyShow "hello", PyShow (\x -> x ** 2)]
allMapStr = map (\(PyShow p) -> show p) [PyShow 1, PyShow 'a', PyShow "hello", PyShow (\x -> x ** 2)]
allNum = [PyShowNum 1, PyShowNum 2.0]