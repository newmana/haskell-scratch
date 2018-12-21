module Lib.MaybeEither where

justA = Just "a"
Just a = justA
Just b = Nothing
maybeToA = maybe "" id justA
isA = a == "a"
isAToo = maybeToA == "a"

eitherC = Right "c"
eitherD = Left "d"
isC = either id id eitherC == "c"
isD = either id id eitherD == "d"

