module Cli
  ( initCLI
  , Cli(..)
  )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )


newtype Cli = Cli
  { file :: String }




initCLI :: Parser Cli
initCLI = Cli <$> argument str (metavar "FILE")


