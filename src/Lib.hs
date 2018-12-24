module Lib
  ( Connection(..)
  , connections
  , getCommonName
  , regularParse
  , connectionParser
  , getHostServiceName
  , isHttps
  ) where

import Data.List (find)
import Network.Socket
import Numeric (readHex)
import OpenSSL.Session
import OpenSSL.X509 (getSubjectName)
import Text.Parsec (ParseError, char, count, endBy, newline, parse)
import Text.Parsec.Char (anyChar, hexDigit)
import Text.Parsec.Combinator (many1, manyTill)
import Text.Parsec.String (Parser, parseFromFile)

data State
  = Established
  | SynSent
  | SynRecv
  | FinWait1
  | FinWait2
  | TimeWait
  | Close
  | CloseWait
  | LastAck
  | Listen
  | Closing
  | NewSyncRecv
  | Unknown
  deriving (Eq, Show)

data Connection = Connection
  { state :: State
  , local :: SockAddr
  , remote :: SockAddr
  }

instance Show Connection where
  show (Connection state local remote) =
    "[" ++ show state ++ "] " ++ show local ++ " -> " ++ show remote

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

stateParser :: Parser State
stateParser = do
  s <- many1 hexDigit
  return $ st $ fst $ head $ readHex s
  where
    st :: Integer -> State
    st 1 = Established
    st 2 = SynSent
    st 3 = SynRecv
    st 4 = FinWait1
    st 5 = FinWait2
    st 6 = TimeWait
    st 7 = Close
    st 8 = CloseWait
    st 9 = LastAck
    st 10 = Listen
    st 11 = Closing
    st 12 = NewSyncRecv
    st _ = Unknown

connectionParser :: Parser Connection
connectionParser = do
  _ <- count 6 anyChar
  local <- socketAddressParser
  char ' '
  remote <- socketAddressParser
  char ' '
  state <- stateParser
  _ <- manyTill anyChar newline
  return Connection {state = state, local = local, remote = remote}

socketAddressParser :: Parser SockAddr
socketAddressParser = do
  addr <- many1 hexDigit
  char ':'
  port <- many1 hexDigit
  return $ SockAddrInet (portNumber port) (hostAddress addr)
  where
    hostAddress addr = fromIntegral $ fst $ head $ readHex addr
    portNumber port = fst $ head $ readHex port

connectionsParser :: Parser [Connection]
connectionsParser = many1 connectionParser

connections :: IO (Either ParseError [Connection])
connections = parseFromFile ignoreFirstLine "/proc/net/tcp"
  where
    ignoreFirstLine = manyTill anyChar newline *> connectionsParser

isHttps' :: Network.Socket.SockAddr -> Bool
isHttps' (Network.Socket.SockAddrInet portNumber _)
  | portNumber == 443 = True
  | portNumber == 8443 = True
  | otherwise = False
isHttps' _ = False

isHttps :: Connection -> Bool
isHttps (Connection _ _ r) = isHttps' r

getHostServiceName :: Network.Socket.SockAddr -> IO (Maybe String, Maybe String)
getHostServiceName = getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True

hints =
  defaultHints
    {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}

newSock :: Network.Socket.AddrInfo -> IO Network.Socket.Socket
newSock i = socket (addrFamily i) (addrSocketType i) (addrProtocol i)

getCommonName :: (Maybe String, Maybe String) -> IO (Maybe String)
getCommonName (hostName, serviceName) = do
  addr:_ <- getAddrInfo (Just hints) hostName serviceName
  cont <- context
  sock <- newSock addr
  Network.Socket.connect sock (addrAddress addr)
  conn <- connection cont sock
  OpenSSL.Session.connect conn
  cert <- getPeerCertificate conn
  case cert of
    Just c -> do
      sn <- getSubjectName c False
      pure $ fmap snd (find (\(x, _) -> x == "CN") sn)
    _ -> pure Nothing
