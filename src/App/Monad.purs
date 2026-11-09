-- | The application monad: `Aff` with a read-only environment holding the
-- | `Backend`. Components run in `AppM`; `Main` hoists them to `Aff`.
module App.Monad
  ( Env
  , AppM
  , runAppM
  ) where

import Prelude

import App.Backend (Backend)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

type Env = { backend :: Backend }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: forall a. Env -> AppM a -> Aff a
runAppM env (AppM m) = runReaderT m env

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk Env AppM
