module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import Network.HTTP.Affjax (AJAX)
import Prelude (bind)
import Pux (renderToDOM, start)
import Signal.Channel (CHANNEL)
-------------------------------------------------------------------------------
import Components.App as App
-------------------------------------------------------------------------------


main
    :: forall eff. Eff ( channel :: CHANNEL
                       , err :: EXCEPTION
                       , ajax :: AJAX
                       | eff) Unit
main = do
  app <- start
    { initialState: App.initialState
    , update: App.update
    , view: App.view
    , inputs: []
    }
  renderToDOM "#app" app.html
