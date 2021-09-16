{-
Count number of words in a file on demand and memoirize the result
-}

import Control.Monad.Reader hiding (ask)
import Control.Monad.State
import Control.Monad.Trans.Reader

import Data.Maybe
import Data.List hiding (lookup)
import qualified Data.Map as Map

type Env = Reader String String
data CounterState = CounterState { content :: String, memoir :: Map.Map String Int }
--type AppT = StateT CounterState IO

counter :: StateT CounterState IO ()
counter = forever $ do
  storedContent <- gets content
  storedMemoir <- gets memoir
  yourName <- liftIO $ putStr "Your name: " >> getLine
  case Map.lookup yourName storedMemoir of
    Just count -> do
      lift $ putStrLn (yourName ++ " (cached): " ++ show count)
    Nothing -> do
      let count = length $ findIndices (\word -> word == yourName) (words storedContent)
      lift $ putStrLn (yourName ++ ": " ++ show count)
      state <- get
      put state { memoir = Map.alter (\_ -> Just count) yourName storedMemoir }

app :: ReaderT String IO ()
app = do
  filename <- ask
  content <- lift $ readFile filename
  lift $ evalStateT counter (CounterState { content = content, memoir = Map.empty })

main :: IO ()
main = do
  runReaderT app "persons.csv"