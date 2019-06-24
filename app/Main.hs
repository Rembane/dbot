{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                            ( void )
import           Options.Applicative                      ( Parser
                                                          , auto
                                                          , execParser
                                                          , fullDesc
                                                          , header
                                                          , help
                                                          , helper
                                                          , info
                                                          , long
                                                          , metavar
                                                          , option
                                                          , optional
                                                          , progDesc
                                                          , short
                                                          , str
                                                          , value
                                                          )

import           Lib

optParser :: Parser Config
optParser =
  Config
    <$> option
          str
          (  long "server"
          <> short 's'
          <> help "The domain or IP address of the server"
          <> metavar "SERVER"
          <> value "irc.dtek.se"
          )
    <*> option
          auto
          (  long "port"
          <> short 'p'
          <> help "The port to connect to"
          <> metavar "PORT"
          <> value 6667
          )
    <*> option
          str
          (  long "channel"
          <> short 'c'
          <> help "The channel to join"
          <> metavar "CHANNEL"
          <> value "#dtek"
          )
    <*> option
          str
          (  long "nick"
          <> short 'n'
          <> help "The nick of the bot"
          <> metavar "NICK"
          <> value "hacke"
          )
    <*> optional
          (option
            str
            (  long "password"
            <> short 'q'
            <> help
                 "The password to register with. Nickname is used as nickname."
            <> metavar "PASSWORD"
            )
          )
    <*> option
          str
          (  long "longname"
          <> short 'l'
          <> help "The long name of the bot"
          <> metavar "LONGNAME"
          <> value "Hacke hackar snabbare!"
          )

main :: IO ()
main
  = (   execParser
        (info
          (helper <*> optParser)
          (fullDesc <> progDesc "The jack of all trades bot for #dtek" <> header
            "Welcome!"
          )
        )
    >>= runClient
    )
    *> void getLine
