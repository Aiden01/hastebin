module Logger
  ( error'
  , success'
  )
where

import           Data.Text.ANSI
import           Data.Text                      ( unpack
                                                , pack
                                                )

error' :: String -> IO ()
error' = putStrLn . unpack . red . pack

success' :: String -> IO ()
success' = putStrLn . unpack . green . pack




