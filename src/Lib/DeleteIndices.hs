{-# LANGUAGE FlexibleContexts #-}

module Lib.DeleteIndices where

import           Control.Lens
import           Data.List
import           Data.Sequence
import           Data.Sequence.Lens


 -- |
 -- >>> deleteIndices [0, 3] (fromList [5,6,7,8])
 -- fromList [6,7]
deleteIndices :: (Num a) => [Int] -> Seq a -> Seq a
deleteIndices indices elems =
  let sorted = Data.List.sort indices
      go [] es     = es
      go (i:is) es = go (fmap pred is) (Data.Sequence.deleteAt i es)
   in go sorted elems

deleteIndices' :: (Foldable t) => t Int -> Seq b -> Seq b
deleteIndices' e = ifoldr (\i a b -> if i `notElem` e then a Data.Sequence.<| b else b) Data.Sequence.empty
