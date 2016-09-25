module Components.App
    ( State(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Pux.Html as H
import Data.Functor (map)
import Network.HTTP.Affjax (AJAX)
import Pux (noEffects, EffModel)
import Pux.Html ((#), div, (!), Html)
import Pux.Html.Attributes (className)
-------------------------------------------------------------------------------
import Components.Counter as Counter
import Components.Now as Now
import Components.List as List
import Components.ListItem as ListItem
-------------------------------------------------------------------------------


type State = {
      nowState :: Now.State
    , counterState :: Counter.State
    , listState :: List.State ListItem.State
    }


initialState :: State
initialState = { nowState: Now.initialState
               , counterState: Counter.initialState
               , listState: List.initialState
               }


data Action = NowAction Now.Action
            | CounterAction Counter.Action
            | ListAction (List.Action ListItem.State ListItem.Action)


update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (NowAction a) s =
  let eff = Now.update a s.nowState
  in { state: s { nowState = eff.state}
     , effects: map (map NowAction) eff.effects}
update (CounterAction a) s = noEffects (s { counterState = Counter.update a s.counterState })
update (ListAction a) s = noEffects (s { listState = List.update a s.listState })


view :: State -> Html Action
view s =
  div ! className "component" # do
    map NowAction (Now.view s.nowState)
    map CounterAction (Counter.view s.counterState)
    map ListAction (List.view {text: "new"} ListItem.view s.listState)
  where
    bind = H.bind
