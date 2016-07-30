-- Only for ghci
module Chemlambda.Pretty where

import Text.Show.Pretty

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow


