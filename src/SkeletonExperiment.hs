module Main where

import RLExperiment

main = do
  runExperiment (\_ -> return ())
