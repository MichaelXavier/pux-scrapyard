module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Components.App as App
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Unit (Unit)
import Network.HTTP.Affjax (AJAX)
import Prelude ((<<<), bind)
import Pux (renderToDOM, start)
--import Pux.Devtool (start)
import Pux.Router (sampleUrl)
import Signal ((~>))
import Signal.Channel (CHANNEL)
-------------------------------------------------------------------------------


main
    :: forall eff. Eff ( channel :: CHANNEL
                       , err :: EXCEPTION
                       , ajax :: AJAX
                       , dom :: DOM
                       | eff) Unit
main = do
  --todo constant signal for ajax refreshes
  app <- start
    { initialState: App.initialState
    , update: App.update
    , view: App.view
    , inputs: []
    }
  renderToDOM "#app" app.html
