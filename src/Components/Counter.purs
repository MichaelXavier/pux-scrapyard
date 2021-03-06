module Components.Counter
    ( State(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Pux.Html as H
import Prelude (show, const, (+), (-))
import Pux.Html ((##), (!), (#), button, text, span, div, Html)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


newtype State = State Int


initialState :: State
initialState = State 0


data Action = Inc
            | Dec

update :: Action -> State -> State
update Inc (State n) = State (n + 1)
update Dec (State n) = State (n - 1)


view :: State -> Html Action
view (State count) =
  div ! className "component" # do
    span # do
      button
        ! onClick (const Dec)
        # text "-"
      text (show count)
      button
        ! onClick (const Inc)
        # text "+"
  where
    bind = H.bind
