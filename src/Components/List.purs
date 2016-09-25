module Components.List
    ( State(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Array ((:), snoc)
import Prelude (const, (<>), (<<<), map, ($))
import Pux.Html (li, ol, button, text, (##), (#), (!), div, Html)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State item = {
      items :: Array item
    }


initialState :: forall item. State item
initialState = { items: [] }


data Action item action = AddItem item
                        | ItemAction action -- uhh, this seems...weird


update :: forall item action. Action item action -> State item -> State item
update (AddItem a) s = s { items = snoc s.items a}
update (ItemAction _) s = s --TODO: would we do indexing to address the right item? is this misguided?

-- item -> Html action
view :: forall item action. item -> (item -> Html action) -> State item -> Html (Action item action)
view newItem viewItem s = div
  [className "component"]
  [ text "list"
  , items
  , addItem
  ]
  where
    items = ol
      []
      (map viewItem' s.items)
    addItem = button
      ! onClick (const (AddItem newItem))
      # text "Add"
    viewItem' i = li [] [map ItemAction (viewItem i)]
