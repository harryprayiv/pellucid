-- | Something fetched once: not here yet, failed, or here.
module App.Loadable
  ( Loadable(..)
  , fromEither
  , toMaybe
  ) where

import App.Backend (AppError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

data Loadable a = Pending | Missing AppError | Ready a

fromEither :: forall a. Either AppError a -> Loadable a
fromEither = case _ of
  Left err -> Missing err
  Right a -> Ready a

toMaybe :: forall a. Loadable a -> Maybe a
toMaybe = case _ of
  Ready a -> Just a
  _ -> Nothing
