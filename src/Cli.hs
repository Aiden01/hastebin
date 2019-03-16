module Cli
  ( initCLI
  , Cli(..)
  )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )


data Cli = Cli
  { file :: String, lines :: Int }




initCLI :: Parser Cli
initCLI = Cli <$> argument str (metavar "FILE") <*> option
  auto
  (  long "lines"
  <> help "Number of lines to take"
  <> showDefault
  <> value 350
  <> metavar "INT"
  )


