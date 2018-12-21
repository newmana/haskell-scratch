-- http://blog.ssanj.net/posts/2014-06-07-trying-to-wrap-a-function-with-a-datatype.html

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Lib.Sanj where

class BuildList a r  | r-> a where
	build' :: [a] -> a -> r

instance BuildList a [a] where
     build' l x = reverse$ x:l

instance BuildList a r => BuildList a (a->r) where
	build' l x y = build'(x:l) y

build x = build' [] x

data TagInfo = TagInfo {
                    fontMin     :: Double,
                    fontMax     :: Double,
                    tagName     :: String,
                    tagUrl      :: String,
                    tagMax      :: Int,
                    tagMin      :: Int,
                    maxUseCount :: Int
               } deriving (Show)

defTags = TagInfo 0.0 0.0 "" "" 1 10 10
a = TagInfo 12.0 12.0 "hi" "there" 1 2 3
b = defTags{tagName="yo"}

renderTagCloud :: (Double -> Double -> String -> String -> Int -> Int -> Int -> String) -> String
renderTagCloud x = "done"

renderSingleLink :: Double -> Double -> String -> String -> Int -> Int -> Int -> String
renderSingleLink = undefined

showTag :: TagInfo -> String
showTag x = "hello"
-- showTag (TagInfo a b c d e f g) = c ++ d
-- showTag . TagInfo
-- renderTagCloudWith ((fmap . fmap . fmap . fmap . fmap . fmap . fmap) showTag TagInfo)
