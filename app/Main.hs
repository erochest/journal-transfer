{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Data.Aeson
import qualified Data.ByteString.Lazy as B

import           Process


main :: IO ()
main = B.interact (either error encode . fmap process . eitherDecode)
