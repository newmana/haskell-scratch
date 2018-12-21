{-# LANGUAGE QuasiQuotes #-}

module Lib.Yeshql where

-- import           Database.HDBC
-- import           Database.YeshQL.HDBC


-- [yesh|
--   -- name:getUser :: (String)
--   -- :userID :: Int
--   SELECT username FROM users WHERE id = :userID
-- |]

main = undefined

-- main = withDB $ \conn -> do
--   username <- getUser 1 conn
--   putStrLn username
