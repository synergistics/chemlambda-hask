-- Only for ghci
module Pretty where

import Text.Show.Pretty

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow


