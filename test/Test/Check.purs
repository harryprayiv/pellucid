-- | The three assertions the tests are written with. A failed check throws,
-- | which fails `spago test`.
module Test.Check
  ( check
  , either'
  , maybe'
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

check :: String -> Boolean -> Effect Unit
check label ok = if ok then log ("ok   " <> label) else throw ("FAIL " <> label)

either' :: forall a. String -> Either String a -> Effect a
either' label = case _ of
  Left err -> throw (label <> ": " <> err)
  Right a -> pure a

maybe' :: forall a. String -> Maybe a -> Effect a
maybe' label = case _ of
  Nothing -> throw (label <> ": missing")
  Just a -> pure a
