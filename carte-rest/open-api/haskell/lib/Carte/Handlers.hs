module Carte.Handlers (carteBackend) where

import Carte.API
import Servant (NoContent (NoContent))
import Control.Monad.Trans (MonadIO)

_healthGet :: MonadIO m  => m NoContent
_healthGet = return NoContent

carteBackend :: MonadIO m => CarteBackend m
carteBackend = CarteBackend {healthGet = _healthGet}
