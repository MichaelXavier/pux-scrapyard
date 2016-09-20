module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Components.Counter as Counter
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import Prelude (bind)
import Pux (renderToDOM, fromSimple, start)
import Signal.Channel (CHANNEL)
-------------------------------------------------------------------------------


main
    :: forall eff. Eff ( channel :: CHANNEL
                       , err :: EXCEPTION
                       | eff) Unit
main = do
  app <- start
    { initialState: Counter.initialState
    , update: fromSimple Counter.update
    , view: Counter.view
    , inputs: []
    }
  renderToDOM "#app" app.html
