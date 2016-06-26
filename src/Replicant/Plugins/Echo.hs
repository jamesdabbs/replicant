module Replicant.Plugins.Echo
  ( echo
  ) where

import Replicant.Plugins.Base
import Data.Attoparsec.Text

echo :: BotM e m => Plugin m
echo = Plugin "echo" [echoH]

echoH :: BotM e m => Handler m
echoH = mkHandler "echo" False ("echo " *> takeText)
  [ Example "echo hello world" "Repeat `hello world` back"
  ]
  reply
