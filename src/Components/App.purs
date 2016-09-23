module Components.App
    ( State(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
import Components.Counter as Counter
import Components.Now as Now
import Pux.Html as H
import Data.Functor (map)
import Network.HTTP.Affjax (AJAX)
import Pux (noEffects, EffModel)
import Pux.Html ((#), div, (!), Html)
import Pux.Html.Attributes (className)
-------------------------------------------------------------------------------


type State = {
      nowState :: Now.State
    , counterState :: Counter.State
    }


initialState :: State
initialState = { nowState: Now.initialState
               , counterState: Counter.initialState
               }


data Action = NowAction Now.Action
            | CounterAction Counter.Action


update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (NowAction a) s =
  let eff = Now.update a s.nowState
  in { state: s { nowState = eff.state}
     , effects: map (map NowAction) eff.effects}
update (CounterAction a) s = noEffects (s { counterState = Counter.update a s.counterState })


view :: State -> Html Action
view s =
  div ! className "component" # do
    map NowAction (Now.view s.nowState)
    map CounterAction (Counter.view s.counterState)
  where
    bind = H.bind
