import Control.Monad.Reader

monadAdd :: (Monad m, Num a) => m a -> m a -> m a
monadAdd ma mb = do
    a <- ma
    b <- mb
    return (a + b)

putFileContent = getLine >>= readFile >>= putStrLn

integerList = [1,2,3]
squaredIntegerList = do
    x <- integerList
    return $ x**2

maybeInteger = Just 3
squaredMaybeInteger = do
    x <- maybeInteger
    return $ x**2

wordCount = print . length . words =<< getContents
countWords = getContents >>= \s -> do return (length (words s))

strToIntReader = reader (read)::Reader String Int
lengthReader = reader (length)::Reader String Int
composedReader :: Reader String (Int, Int)
composedReader =
  strToIntReader >>= \x ->
  lengthReader >>= \y ->
  return (x, y)


main :: IO ()
main = do
  let hundred = runReader composedReader "100"
  putStrLn (show hundred)