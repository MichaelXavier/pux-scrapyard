module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Components.Counter as Counter
import Components.ISS as ISS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import Network.HTTP.Affjax (AJAX)
import Prelude (bind)
import Pux (renderToDOM, fromSimple, start)
import Signal.Channel (CHANNEL)
-------------------------------------------------------------------------------


main
    :: forall eff. Eff ( channel :: CHANNEL
                       , err :: EXCEPTION
                       , ajax :: AJAX
                       | eff) Unit
main = do
  counter <- start
    { initialState: Counter.initialState
    , update: fromSimple Counter.update
    , view: Counter.view
    , inputs: []
    }
  renderToDOM "#counter" counter.html

  iss <- start
    { initialState: ISS.initialState
    , update: ISS.update
    , view: ISS.view
    , inputs: []
    }
  renderToDOM "#iss" iss.html
