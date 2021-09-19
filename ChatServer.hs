{- ChatServer
- Accepts connection from telnet clients
- Receives text message from client socket
- Relay text message to other connected client threads via Chan String
- Receives message from Chan and send back to client socket

References:
- https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html
- https://wiki.haskell.org/Implement_a_chat_server
-}
module Main (main) where
import System.IO

import Control.Concurrent
import Control.Monad.State
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as Bytechar

data ChatClient = ChatClient { sockClient :: Socket, channel :: Chan String }

chatClient :: StateT ChatClient IO ()
chatClient = do
  sock <- gets sockClient
  sharedChan <- gets channel
  duplicatedChan <- lift $ dupChan sharedChan
  let broadcast msg = writeChan duplicatedChan msg

  lift $ forkIO $ fix $ \loop -> do
    line <- readChan duplicatedChan
    sendAll sock (Bytechar.pack $ ">> " ++ line)
    loop

  lift $ fix $ \loop -> do
    line <- recv sock 1024
    broadcast (Bytechar.unpack line)
    loop

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    -- talk s :: StateT ChatClient IO String
    talk channel s =
      let initialState = ChatClient { sockClient = s, channel = channel }
      in evalStateT chatClient initialState


-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Chan String -> Socket -> IO ()) -> IO ()
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  chan <- newChan -- notice that newChan :: IO (Chan a)
  -- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
  -- bracket is the monad exception handling: do (IO a) and finally release (a -> IO b)
  --   the main action is (a -> IO c)
  -- expressed in try..catch as
  -- try (IO a) return (IO c) catch (IO b)
  E.bracket (open addr) close (loop chan)
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
    loop chan sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (void $ server chan conn) (const $ gracefulClose conn 5000)
