{-# LANGUAGE FlexibleContexts #-}
module Replicant.Plugins.Score
  ( score
  ) where

import Replicant.Plugins.Base

import qualified Data.Text                as T
import qualified Database.Redis.Namespace as R


score :: Replicant e m => Plugin m
score = Plugin "score" [scoreUp, scoreDown, scoreShow]


scoreShow, scoreUp, scoreDown :: Replicant e m => Handler m

scoreShow = mkHandler "Show score" False ("score " *> word)
  [ Example "score leeloo" "Show the score for leeloo"]
  $ \term -> delta term 0

scoreUp = mkHandler "Add score" False (word <* "++")
  [ Example "leeloo++" "Give leeloo 1 point"]
  $ \term -> delta term 1

scoreDown = mkHandler "Lower score" False (word <* "--")
  [ Example "leeloo--" "Be a monster"]
  $ \term -> delta term (-1)


delta :: Replicant e m => Text -> Integer -> H m ()
delta term dn = redis (R.incrby term' dn) >>= explain term
  where term' = encodeUtf8 term

explain :: Monad m => Text -> Integer -> H m ()
explain term n = reply $ term <> " has " <> T.pack (show n) <> " points"
