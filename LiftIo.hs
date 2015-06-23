--
-- Working example of http://en.wikibooks.org/wiki/Haskell/Monad_transformers
-- can replace lift with lifeIO

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans (liftIO)

askPassphrase :: MaybeT IO ()
askPassphrase = do 
    liftIO $ putStrLn "Insert your new passphrase:"
    value <- getValidPassphrase
    liftIO $ putStrLn "Storing in database..."

getValidPassphrase :: MaybeT IO String
getValidPassphrase = do 
    s <- liftIO getLine
    guard (isValid s)
    return s
 
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

pathsWriterT' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
    let e_paths = do 
        (e_start, e_end) <- lift edges
        guard $ e_start == start
        tell [start]
        pathsWriterT' edges e_end end
    in if start == end then tell [start] `mplus` e_paths else e_paths

pathsWriterT :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)

readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do 
    x <- ask
    lift . tell $ show x
    return $ x + 1

graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]
doPaths = pathsWriterT graph1 2013 2558