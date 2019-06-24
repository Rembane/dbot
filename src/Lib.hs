{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Lib where

import           Control.Applicative                      ( optional )
import           Control.Arrow                            ( (>>>) )
import           Control.Concurrent                       ( MVar
                                                          , newEmptyMVar
                                                          , takeMVar
                                                          , tryPutMVar
                                                          )
import           Control.Concurrent.Async                 ( async
                                                          , link
                                                          , wait
                                                          )
import           Control.Monad                            ( (>=>)
                                                          , (<=<)
                                                          , void
                                                          , when
                                                          )
import           Data.Attoparsec.ByteString.Char8         ( IResult(..)
                                                          , Parser
                                                          , Result
                                                          , choice
                                                          , parse
                                                          , skipSpace
                                                          , string
                                                          , takeWhile1
                                                          )
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.List                                ( intercalate )
import           Data.Maybe                               ( listToMaybe
                                                          , mapMaybe
                                                          )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding                       ( encodeUtf8 )
import           Network.Connection                       ( Connection
                                                          , ConnectionParams(..)
                                                          , connectionGetChunk'
                                                          , connectionPut
                                                          , connectTo
                                                          , initConnectionContext
                                                          )
import           Network.HTTP.Req                         ( GET(..)
                                                          , NoReqBody(..)
                                                          , Option
                                                          , Url
                                                          , defaultHttpConfig
                                                          , lbsResponse
                                                          , parseUrl
                                                          , req
                                                          , responseBody
                                                          , runReq
                                                          )
import           Network.Socket                           ( PortNumber )
import           System.IO                                ( hFlush
                                                          , stdout
                                                          )
import           Text.HTML.TagSoup                        ( isTagCloseName
                                                          , isTagOpenName
                                                          , maybeTagText
                                                          , parseTags
                                                          )

data Config = Config
  { server   :: Text
  , port     :: PortNumber
  , chan     :: Text
  , nick     :: Text
  , password :: Maybe Text
  , longname :: Text
  }

data Bot = Bot
  { connection   :: Connection
  , config       :: Config
  , isHandshaked :: MVar ()
  }

data Command
  = ReplyWelcome B8.ByteString
  | Ping B8.ByteString
  | Privmsg B8.ByteString B8.ByteString B8.ByteString -- ^ Sender, Target, message
  | Other B8.ByteString
  deriving (Eq, Show)

-- | Write a ByteString to the connection.
-- All IRC commands end with \r\n (end of line), which is why we add it here.
writeRaw :: Connection -> B8.ByteString -> IO ()
writeRaw c s = connectionPut c s' *> B8.putStr s' where s' = s <> "\r\n"

-- | Write Text to the connection.
write :: Connection -> Text -> IO ()
write c = writeRaw c . encodeUtf8

-- | Parse exactly one line and turn it into a Command.
commandParser :: Config -> Parser Command
commandParser cfg =
  optional (string "\r\n")
    *> skipSpace
    *> optional (":" *> string (encodeUtf8 $ server cfg))
    *> skipSpace
    *> choice
         [ Ping <$> ("PING :" *> takeWhile1 (/= '\r'))
         , ReplyWelcome <$> ("001 " *> takeWhile1 (/= '\r'))
         , Privmsg
         <$> (":" *> takeWhile1 (/= ' ')) -- Sender
         <*> (skipSpace *> "PRIVMSG" *> skipSpace *> takeWhile1 (/= ' ')) -- Target
         <*> (skipSpace *> ":" *> takeWhile1 (/= '\r')) -- Message
         , Other <$> takeWhile1 (/= '\r')
         ]
    <* string "\r\n"

-- | Gets all of the URLs in the string.
--
-- We use a space as a delimiter between URLs.
allUrls :: B8.ByteString -> [B8.ByteString]
allUrls = filter (B8.isPrefixOf "http") . B8.split ' '

-- | Given the content of a homepage, this function gives the title of that page
-- if there is one.
parseTitle :: BL.ByteString -> Maybe B8.ByteString
parseTitle =
  (   parseTags
    >>> dropWhile (not . isTagOpenName "title")
    >>> drop 1 -- Drop the title opening tag too, we don't need it.
    >>> Prelude.takeWhile (not . isTagCloseName "title")
    >>> listToMaybe
    )
    >=> (fmap BL.toStrict . maybeTagText)

-- | Given an URL, this function fetches everything between
-- <title> and </title> for that homepage and writes it to
-- the IRC socket.
fetchTitle
  :: Bot
  -> Either (Url scheme, Option scheme) (Url scheme1, Option scheme1)
  -> IO ()
fetchTitle bot =
  either f f
    >=> (responseBody >>> parseTitle >>> maybe
          (pure ())
          ( writeRaw (connection bot)
          . (("PRIVMSG " <> encodeUtf8 (chan (config bot)) <> " :Title: ") <>)
          )
        )
 where
  f (url, opts) =
    runReq defaultHttpConfig (req GET url NoReqBody lbsResponse opts)

-- | Listen for new messages and handle them appropriately.
listen :: Bot -> Maybe (B8.ByteString -> Result Command) -> IO ()
listen bot parser =
  connectionGetChunk'
      (connection bot)
      (\s -> case maybe (parse (commandParser $ config bot) s) ($ s) parser of
        f@Fail{}     -> (f, "")
        p@Partial{}  -> (p, "")
        d@(Done i _) -> (d, i)
      )
    >>= \case
          Fail i cs e ->
            putStrLn
              $  "FAIL!\nConsumed: "
              <> show i
              <> ",\nContexts: "
              <> intercalate "\n" cs
              <> "\nError: "
              <> e
          Partial f -> putStrLn "Partial!" *> listen bot (Just f)
          Done _ r ->
            putStrLn ("Success!: " <> show r)
              *> case r of
                   ReplyWelcome _ -> void (tryPutMVar (isHandshaked bot) ())
                   Ping s -> writeRaw (connection bot) ("PONG :" <> s)
                   Privmsg _ target msg -> when
                     (B8.isInfixOf (encodeUtf8 $ chan $ config bot) target)
                     (mapM_ (link <=< (async . fetchTitle bot))
                            (mapMaybe parseUrl (allUrls msg))
                     )
                   _ -> pure ()
              *> listen bot Nothing

-- | Connect to the server, identify the bot and return the initial bot state.
runClient :: Config -> IO ()
runClient cfg = do
  putStrLn ("Connecting to " <> show (server cfg) <> "...")
  hFlush stdout
  c <-
    initConnectionContext
      >>= (`connectTo` (ConnectionParams
                         { connectionHostname  = T.unpack $ server cfg
                         , connectionPort      = port cfg
                         , connectionUseSecure = Nothing
                         , connectionUseSocks  = Nothing
                         }
                       )
          )
  putStrLn "Connected."
  ihs          <- newEmptyMVar
  listenAction <- async (listen (Bot c cfg ihs) Nothing)
  write c $ "NICK " <> nick cfg
  write c $ "USER " <> nick cfg <> " 0 * :" <> longname cfg
  void $ takeMVar ihs
  maybe (pure ())
        (write c . (("PRIVMSG NickServ :identify " <> nick cfg <> " ") <>))
        (password cfg)
  write c $ "JOIN " <> chan cfg
  wait listenAction
