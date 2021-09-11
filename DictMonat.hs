import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Bindings = Map.Map String String

{- The hidden param is the "Bindings" itself. It is like the resultant <Reader Bindings (Maybe String)> dictionary
is a function whose one single parameter is the "missing" Bindings.

-}
dictionary :: String -> Reader Bindings (Maybe String)
dictionary key = Map.lookup key <$> ask
  {- The original imperative approach: -}
  -- do
  --   bindings <- ask
  --   let value = Map.lookup key bindings
  --   return value

  {- A more Haskell way -}
  -- do
  --   value <- Map.lookup key <$> ask
  --   return value
  {- OR we could kick ass with the <$> FUNCTOR -}
  -- (<$>) :: Functor f => (a -> b) -> f a -> f b
  -- ask :: MonadReader r m => m r

foodChain :: String -> Reader Bindings String
foodChain top = do
  food <- dictionary top
  case food of
    Nothing -> return top
    Just value -> foodChain value


main :: IO ()
main = do
  let definitions = Map.fromList [("Man", "Cow"), ("Cow", "Grass"), ("Grass", "Earth")]
  putStrLn "Food Chain"
  putStrLn $ runReader (foodChain "Man") definitions
