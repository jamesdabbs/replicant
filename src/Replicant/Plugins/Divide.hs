module Replicant.Plugins.Divide
  ( divide
  ) where

import           Replicant.Plugins.Base
import           Data.Attoparsec.Text
import qualified Data.Text as T

divide :: Replicant e m => Plugin m
divide = Plugin "divide" [divideH]

divideH :: Replicant e m => Handler m
divideH = mkHandler "divide" True divParser
  [ Example "10 / 2" "Do some math"
  , Example "1 / 0" "Force a crash"
  ]
  $ \(num, denom) -> reply . T.pack . show $ num `div` denom

divParser :: Parser (Int, Int)
divParser = do
  a <- many digit
  whitespace
  _ <- string "/"
  whitespace
  b <- many digit
  return (read a, read b)
