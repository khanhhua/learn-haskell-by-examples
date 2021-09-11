import Control.Monad.Reader

-- The Reader/IO combined monad, where Reader stores a string.
printReaderContent :: ReaderT String IO ()
printReaderContent =
    ask >>= \content -> ($) liftIO (putStrLn ("The Reader Content: " ++ content))

main = do
    runReaderT printReaderContent "Some Content"
--calculateContentLen :: Reader String Int
--calculateContentLen = do
--    content <- ask
--    return (length content);
--
---- Calls calculateContentLen after adding a prefix to the Reader content.
--calculateModifiedContentLen :: Reader String Int
--calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen
--
--main = do
--    let s = "12345";
--    let modifiedLen = runReader calculateModifiedContentLen s
--    let len = runReader calculateContentLen s
--    putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
--    putStrLn $ "Original 's' length: " ++ (show len)
