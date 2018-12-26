module Lib
  ( Connection(..)
  , connections
  , getCommonName
  , regularParse
  , connectionParser
  , getHostServiceName
  , isHttps
  ) where

import Data.Binary.Get
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List (find)
import Data.Word
import Network.Socket
import Numeric (readHex)
import OpenSSL.Session
import OpenSSL.X509 (getSubjectName)
import Text.Parsec (ParseError, char, count, endBy, newline, parse)
import Text.Parsec.Char (anyChar, hexDigit)
import Text.Parsec.Combinator (many1, manyTill)
import Text.Parsec.String (Parser, parseFromFile)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy.Char8 as Char8

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
    "[" ++ show state ++ "] " ++ show local ++ " -> " ++ show remote ++ "\n"

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

decodeHex :: String -> ByteString.ByteString
decodeHex str =
  let (x, _) = Base16.decode (toStrict (Char8.pack str))
   in x

des :: Get (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
des = do
  fir <- getWord16le
  sec <- getWord16le
  thi <- getWord16le
  fou <- getWord16le
  fif <- getWord16le
  six <- getWord16le
  sev <- getWord16le
  eig <- getWord16le
  return (sec, fir, fou, thi, six, fif, eig, sev)

socketAddressParser :: Parser SockAddr
socketAddressParser = do
  addr <- many1 hexDigit
  char ':'
  port <- many1 hexDigit
  return $ sockAddr addr port
  where
    hostAddress addr = fromIntegral $ fst $ head $ readHex addr
    portNumber port = fst $ head $ readHex port
    hostAddress6 addr =
      tupleToHostAddress6 (runGet des (fromStrict (decodeHex addr)))
    sockAddr addr port =
      if length addr == 32
        then SockAddrInet6 (portNumber port) 0 (hostAddress6 addr) 0
        else SockAddrInet (portNumber port) (hostAddress addr)

connectionsParser :: Parser [Connection]
connectionsParser = many1 connectionParser

connections' :: String -> IO (Either ParseError [Connection])
connections' = parseFromFile ignoreFirstLine
  where
    ignoreFirstLine = manyTill anyChar newline *> connectionsParser

connections = do
  tcp <- connections' "/proc/net/tcp"
  tcp6 <- connections' "/proc/net/tcp6"
  case (tcp, tcp6) of
    (Right a, Right b) -> return $ concat [a, b]
    (Left _, Right b) -> return b
    (Right a, Left _) -> return a
    _ -> return []

isHttps' :: Network.Socket.SockAddr -> Bool
isHttps' (Network.Socket.SockAddrInet portNumber _)
  | portNumber == 443 = True
  | portNumber == 8443 = True
  | otherwise = False
isHttps' (Network.Socket.SockAddrInet6 portNumber _ _ _)
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
  ctx <- context
  sock <- newSock addr
  Network.Socket.connect sock (addrAddress addr)
  conn <- connection ctx sock
  OpenSSL.Session.connect conn
  cert <- getPeerCertificate conn
  case cert of
    Just c -> do
      sn <- getSubjectName c False
      pure $ fmap snd (find (\(x, _) -> x == "CN") sn)
    _ -> pure Nothing
