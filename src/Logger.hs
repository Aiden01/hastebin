module Logger
  ( error'
  , success'
  )
where

import           Data.Text.ANSI
import           Data.Text                      ( unpack
                                                , pack
                                                )

error' :: Bool -> String -> IO ()
error' True _   = return ()
error' _    str = putStrLn $ unpack $ red $ pack str

success' :: Bool -> String -> IO ()
success' True _   = return ()
success' _    str = putStrLn $ unpack $ green $ pack str



