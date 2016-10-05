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
import Pux (renderToDOM)
import Pux.Devtool (start)
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
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> App.match
  app <- start
    { initialState: App.initialState
    , update: App.update
    , view: App.view
    , inputs: [routeSignal]
    }
  renderToDOM "#app" app.html
