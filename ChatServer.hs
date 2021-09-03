module Main (main) where

import Control.Monad.State
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as Bytechar

data ChatClient = ChatClient { sockClient :: Socket, chats :: [String] }

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    -- talk s :: StateT ChatClient IO ()
    talk s =
      let initialState = ChatClient { sockClient = s, chats = [] }
      in runStateT talk' initialState >> return ()

    talk' :: StateT ChatClient IO ()
    talk' = do
      -- let s = sockClient state
      s <- sockClient <$> get
      chatItems <- chats <$> get

      msg <- liftIO $ recv s 1024

      unless (S.null msg) $ do
        liftIO $ putStrLn (show chatItems)
        put ChatClient { sockClient = s, chats = [(Bytechar.unpack msg)] ++ chatItems }
        liftIO $ do
          Bytechar.putStrLn msg
          sendAll s msg
      talk'


-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO ()) -> IO ()
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  -- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
  -- brack is the monad exception handling: do (IO a) and finally release (a -> IO b)
  --   the main action is (a -> IO c)
  -- expressed in try..catch as
  -- try (IO a) return (IO c) catch (IO b)
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      -- addrs <- getAddrInfo (Just hints) mhost (Just port)
      -- return (head addrs)
      -- head :: [a] -> a
      -- (<$>) :: Functor f => (a -> b) -> f a -> f b
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (void $ server conn) (const $ gracefulClose conn 5000)