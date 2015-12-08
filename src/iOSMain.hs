{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import HXNetworking

foreign export ccall "haskell_main" main :: IO ()
main = moduleMain
