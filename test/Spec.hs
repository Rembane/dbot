{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.ByteString.Char8         ( maybeResult
                                                          , parse
                                                          )
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Char8    as BL
import           Test.Tasty                               ( defaultMain
                                                          , testGroup
                                                          )
import           Test.Tasty.HUnit                         ( (@?=)
                                                          , testCase
                                                          )

import           Lib

main :: IO ()
main = defaultMain $ testGroup
  "Lib"
  [ testGroup
    "Testing allUrls"
    [ testCase "Parse one URL!"
    $   allUrls "http://www.sunet.se/"
    @?= ["http://www.sunet.se/" :: B8.ByteString]
    , testCase "Parse a URL surrounded by stuff"
    $   allUrls "stuff http://www.sunet.se/ other stuff"
    @?= ["http://www.sunet.se/" :: B8.ByteString]
    , testCase "Parse many URLs interleaved with text"
    $   allUrls
          "stuff http://www.sunet.se/ other stuff https://www.example.com bla https://yahoo.com"
    @?= [ "http://www.sunet.se/" :: B8.ByteString
        , "https://www.example.com"
        , "https://yahoo.com"
        ]
    , testCase "No matching URL"
    $   allUrls "stuff other stuff minor distractions hhhh"
    @?= []
    ]
  , testGroup
    "Testing title parsing"
    [ testCase "No parse of empty document" $ parseTitle "" @?= Nothing
    , testCase "Minimal webpage"
    $   parseTitle
          (mconcat
            [ "<!DOCTYPE html>"
            , "<html lang=\"en\">"
            , "  <head>"
            , "    <meta charset=\"utf-8\">"
            , "    <title>This is the title!</title>"
            , "    <link rel=\"stylesheet\" href=\"style.css\">"
            , "    <script src=\"script.js\"></script>"
            , "  </head>"
            , "  <body>"
            , "    <h1>A minimal web page according to Sitepoint</h1>"
            , "    <a href=\"https://www.sitepoint.com/a-minimal-html-document-html5-edition/\">"
            , "      https://www.sitepoint.com/a-minimal-html-document-html5-edition/"
            , "    </a>"
            , "  </body>"
            , "</html>"
            ] :: BL.ByteString
          )
    @?= Just "This is the title!"
    ]
  , testGroup
    "Testing Command parsers"
    [ testCase "Reply welcome"
    $   parser
          cfg
          ":my-fancy-server.example.com 001 hacke :Welcome to the example.com IRC Network hacke!hacke@123-456-78-90-no12346.tbcn.telia.com\r\n"
    @?= Just
          (ReplyWelcome
            "hacke :Welcome to the example.com IRC Network hacke!hacke@123-456-78-90-no12346.tbcn.telia.com"
          )
    , testCase "Ping"
    $   parser cfg "PING :my-fancy-server.example.com\r\n"
    @?= Just (Ping "my-fancy-server.example.com")
    , testCase "Privmsg"
    $   parser
          cfg
          ":my-fancy-server.example.com :Equeroot!irssi@li666-666.members.linode.com PRIVMSG #dtek-spam :Hej!\r\n"
    @?= Just
          (Privmsg "Equeroot!irssi@li666-666.members.linode.com"
                   "#dtek-spam"
                   "Hej!"
          )
    ]
  ]
 where
  emptyConfig = Config
    { server   = ""
    , port     = 1025
    , chan     = ""
    , nick     = ""
    , password = Nothing
    , longname = ""
    }
  cfg = emptyConfig { server = "my-fancy-server.example.com" }
  parser c = maybeResult . parse (commandParser c)
