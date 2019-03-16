module Cli
  ( initCLI
  , Cli(..)
  )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )


data Cli = Cli
  { file :: String, quiet :: Bool }




initCLI :: Parser Cli
initCLI = Cli <$> argument str (metavar "FILE") <*> switch
  (long "quiet" <> short 'q' <> help "Whether to show the logs")


