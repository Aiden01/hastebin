module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import qualified Cli                           as App
import qualified Network.Wreq                  as Http
import           Control.Lens
import           Data.Aeson.Lens
import           Data.Aeson
import           Data.Map                hiding ( take )
import           Data.Text               hiding ( concat
                                                , take
                                                , lines
                                                )
import           Logger
import           System.Directory               ( doesFileExist )
import qualified Data.ByteString.Char8         as B



data Status = Success | Error String deriving (Show)
type Resp = Http.Response (Map String Value)

main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    (App.initCLI <**> helper)
    (  fullDesc
    <> progDesc "A CLI for hastebin written in Haskell"
    <> header
         "hastebin-cli - Upload files from your command line directly to hastebin"
    )


endpoint :: String
endpoint = "https://hastebin.com/"




-- Uploads the file to hastebin
uploadFile :: String -> IO (Maybe Text)
uploadFile buffer = do
  response <-
    Http.asJSON =<< Http.post (endpoint ++ "documents") (B.pack buffer) :: IO
      Resp
  let body = response ^. Http.responseBody
  return (body ! "key" ^? _String)



run :: App.Cli -> IO ()
run (App.Cli file q) = do
  fileExists <- doesFileExist file
  if fileExists
    then do
      contents <- readFile file
      body     <- uploadFile contents
      case body of
        Nothing  -> error' q "An error occurred"
        Just key -> success'
          q
          (  "The file has been uploaded successfully. Url: "
          ++ endpoint
          ++ unpack key
          )
    else error' q ("File " ++ file ++ "does not exist")

run _ = return ()
