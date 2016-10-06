module Components.AJAXList
    ( State(..)
    , AJAXListItem(..)
    , ItemStatus(..)
    , ListStatus(..)
    , initialState
    , Action(..)
    , ItemAction(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Array as A
import Data.Map as M
import Pux.Html as H
import Control.Monad.Aff (attempt)
import Data.Argonaut ((.?), class DecodeJson, decodeJson)
import Data.Either (Either(Right, Left), either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Prelude (const, pure, show, (<<<), map, bind)
import Pux (noEffects, EffModel)
import Pux.Html (button, text, (!), li, ol, (#), div, Html)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State = {
      items :: M.Map Int AJAXListItem
    , status :: ListStatus
    }

data ListStatus = ListNotFetched
                | ListFetching
                | ListFetched
                | ListFetchError String


type AJAXListItem = {
      text :: String
    , newText :: Maybe String
    , status :: ItemStatus
    , id :: Int
    }

newtype RawAJAXListItem = RawAJAXListItem {
      text :: String
    , id :: Int
    }


instance decodeJsonRawAJAXListItem :: DecodeJson RawAJAXListItem where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "text"
    i <- obj .? "id"
    pure (RawAJAXListItem {text: t, id: i})


fromRawAJAXListItem :: RawAJAXListItem -> AJAXListItem
fromRawAJAXListItem (RawAJAXListItem i) = {
      text: i.text
    , newText: Nothing
    , status: ItemCreated
    , id: i.id
    }


data ItemStatus = ItemCreated
                | ItemDeleting
                | ItemDeleted


initialState :: State
initialState = { items: mempty, status: ListNotFetched }


data Action = RefreshList
            | ReceiveList (Either String (M.Map Int AJAXListItem))
            | ItemAction Int ItemAction


data ItemAction = DeleteItem


mapItemsById
    :: Array AJAXListItem
    -> M.Map Int AJAXListItem
mapItemsById = M.fromFoldable <<< map toPair
  where
    toPair i = Tuple i.id i


update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (ReceiveList (Left e)) s =
  noEffects (s { status = ListFetchError e})
update (ReceiveList (Right items)) s =
  noEffects (s { items = items, status = ListFetched})
update RefreshList s = {
      state: s { status = ListFetching }
    , effects: [do
      res <- attempt (get "/items.json")
      let decode reply = decodeJson reply.response
      let t = either (Left <<< show) (map (mapItemsById <<< (map fromRawAJAXListItem)) <<< decode) res
      pure (ReceiveList t)
      ]
    }
-- TODO: yikes! actually get a failed pattern match where it tries to match a DeleteItem to a ItemAction i DeleteItem, pux bug?
update (ItemAction id a) s = noEffects s { items = M.update updateItem' id s.items}
  where
    updateItem' { status: ItemDeleted} = Nothing
    updateItem' itemStatus = Just (updateItem a itemStatus)


--TODO: ajax to actually delete from server
updateItem :: ItemAction -> AJAXListItem -> AJAXListItem
updateItem DeleteItem i = i { status = ItemDeleting }


--TODO: how do we do an initial load
view :: State -> Html Action
view s = div
  ! className "component"
  # do
    items
    button ! onClick (const RefreshList) # text "Refresh"
  where
    bind = H.bind
    items = case s.status of
      ListNotFetched -> text "Not fetched"
      ListFetching -> do
        text "Fetching"
        items'
      ListFetched -> items'
      ListFetchError e -> do
        text ("Error: " <> e)
        items'
    items' = ol
      []
      (A.fromFoldable (map viewItem' (M.values s.items)))
    viewItem' i = li
      # do
        map (ItemAction i.id) (viewItem i)


--TODO: more detail
viewItem :: AJAXListItem -> Html ItemAction
viewItem { status: ItemDeleting } = text "Deleting..."
viewItem i = do
  text i.text
  button
    ! onClick (const DeleteItem)
    # text "Delete"
  where
    bind = H.bind
