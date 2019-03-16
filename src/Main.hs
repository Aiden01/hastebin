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



data Status = Success | Error String deriving (Show)
type Resp = Http.Response (Map String Value)

main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    (App.initCLI <**> helper)
    (fullDesc <> progDesc "A CLI for hastebin written in Haskell" <> header
      "hastebin-cli - Upload file from your command line directly to hastebin"
    )


endpoint :: String
endpoint = "https://hastebin.com/"




-- Uploads the file to hastebin
uploadFile :: IO [String] -> IO (Maybe Text)
uploadFile buffer = do
  contents <- concat <$> buffer
  response <-
    Http.asJSON =<< Http.post (endpoint ++ "documents") (toJSON contents) :: IO
      Resp
  let body = response ^. Http.responseBody
  return (body ! "key" ^? _String)


-- Reads the file and returns its content
getFileContents :: String -> Int -> IO [String]
getFileContents path n = take n . lines <$> readFile path

run :: App.Cli -> IO ()
run (App.Cli file n) = do
  fileExists <- doesFileExist file
  if fileExists
    then do
      body <- uploadFile $ getFileContents file n
      case body of
        Nothing -> error' "An error occurred"
        Just key ->
          success'
            $  "The file has been uploaded successfully. Url: "
            ++ endpoint
            ++ unpack key
    else error' $ "File " ++ file ++ "does not exist"

run _ = return ()
