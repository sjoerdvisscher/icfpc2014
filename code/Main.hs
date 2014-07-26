module Main where

import CompileGCC
import CompileGHC

main :: IO ()
main = do

  hs <- readFile "../solution/lambdaman.hs"
  gcc <- compileGCC hs
  writeFile "../solution/lambdaman.gcc" gcc

  js <- readFile "../solution/ghost0.js"
  ghc <- compileGHC js
  writeFile "../solution/ghost0.ghc" ghc
