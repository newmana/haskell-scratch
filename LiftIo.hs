--
-- Working example of http://en.wikibooks.org/wiki/Haskell/Monad_transformers
-- can replace lift with lifeIO

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans (liftIO)

getValidPassphrase :: MaybeT IO String
getValidPassphrase = do 
    s <- liftIO getLine
    guard (isValid s)
    return s
 
askPassphrase :: MaybeT IO ()
askPassphrase = do 
    liftIO $ putStrLn "Insert your new passphrase:"
    value <- getValidPassphrase
    liftIO $ putStrLn "Storing in database..."

getPassphrase :: IO (Maybe String)
getPassphrase = do 
	s <- getLine
	if isValid s then return $ Just s else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

isAlpha :: Char -> Bool
isAlpha a = True

isNumber :: Char -> Bool
isNumber a = True

isPunctuation :: Char -> Bool
isPunctuation a = True	
