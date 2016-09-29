module Components.List
    ( State(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Components.ListItem as ListItem
import Data.Array (mapMaybe, (:), snoc)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (otherwise, (+), show, const, (<>), (<<<), map, ($), (==))
import Pux.Html (li, ol, button, text, (##), (#), (!), div, Html)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State = {
      items :: Array ListItem.State
    , id :: Int
    }


initialState :: State
initialState = { items: [], id: 0 }


data Action = AddItem String
            | ItemAction Int ListItem.Action -- uhh, this seems...weird


update :: Action -> State -> State
update (AddItem a) s = s { items = snoc s.items (ListItem.initialState s.id a)
                         , id = s.id + 1}
update (ItemAction id a) s = s { items = mapMaybe go s.items }
  where
    go itemState
      | itemState.id == id = go' (ListItem.update a itemState)
      | otherwise          = go' itemState
    go' itemState
      | itemState.deleted = Nothing
      | otherwise         = Just itemState


-- item -> Html action
view :: State -> Html Action
view s = div
  [className "component"]
  [ text "list"
  , items
  , addItem
  ]
  where
    curId = s.id
    items = ol
      []
      (map viewItem' s.items)
    addItem = button
      ! onClick (const (AddItem ("new " <> show curId)))
      # text "Add"
    viewItem' :: ListItem.State -> Html Action
    viewItem' i = li [] [map (ItemAction i.id) (ListItem.view i)]
