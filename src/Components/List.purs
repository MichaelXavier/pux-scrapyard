module Components.List
    ( State(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Components.ListItem as ListItem
import Data.Map as M
import Data.Array as A
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Prelude (otherwise, (+), show, const, (<>), (<<<), map, ($), (==))
import Pux.Html (li, ol, button, text, (##), (#), (!), div, Html)
import Pux.Html as H
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State = {
      items :: M.Map Int ListItem.State
    , id :: Int
    }


initialState :: State
--initialState = { items: mempty, id: 0 }
initialState = { items: M.singleton 0 (ListItem.initialState 0 "newone"), id: 0 }


-- data Action = AddItem String
--             | ItemAction Int ListItem.Action

data Action = ItemAction Int ListItem.Action


--FIXME: this is actually broken!
update :: Action -> State -> State
-- update (AddItem a) s = s { items = M.insert s.id (ListItem.initialState s.id a) s.items
--                          , id = s.id + 1}
update (ItemAction id a) s = s { items = M.update go id s.items }
  where
    -- apply the item state change first and then deal with it
    go itemState = go' (ListItem.update a itemState)
    go' itemState
      | itemState.deleted = Nothing
      | otherwise         = Just (ListItem.update a itemState )


-- item -> Html action
view :: State -> Html Action
view s = div
  ! className "component"
  # do
    text "list"
    items
    -- addItem
  where
    bind = H.bind
    curId = s.id
    items = ol
      []
      (A.fromFoldable (map viewItem' (M.values s.items)))
    -- addItem = button
    --   ! onClick (const (AddItem ("new " <> show curId)))
    --   # text "Add"
    viewItem' :: ListItem.State -> Html Action
    viewItem' i = li [] [map (ItemAction i.id) (ListItem.view i)]
