module Components.AJAXList
    ( State(..)
    , Status(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Components.AJAXListItem as AJAXListItem
import Data.Array as A
import Data.Map as M
import Control.Monad.Aff (attempt)
import Data.Argonaut (decodeJson)
import Data.Either (Either(Right, Left), either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX, get)
import Prelude (map, pure, show, bind, (<<<))
import Pux (noEffects, EffModel)
import Pux.Html (Html, div, li, ol, text, (##), (!), (#))
import Pux.Html.Attributes (className)
-------------------------------------------------------------------------------


type State = {
      items :: M.Map Int AJAXListItem.State
    , status :: Status
    }

data Status = ListNotFetched
            | ListFetching
            | ListFetched
            | ListFetchError String


initialState :: State
initialState = { items: mempty, status: ListNotFetched }


data Action = RefreshList
            | ReceiveList (Either String (M.Map Int AJAXListItem.State))
            | AJAXItemAction Int AJAXListItem.Action


mapItemsById
    :: Array AJAXListItem.State
    -> M.Map Int AJAXListItem.State
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
      let t = either (Left <<< show) (map (mapItemsById <<< (map AJAXListItem.fromRawState)) <<< decode) res
      pure (ReceiveList t)
      ]
    }
update (AJAXItemAction id a) s = case M.lookup id s.items of
  Just itemState -> let eff = AJAXListItem.update a itemState
                    in { state: s { items = M.insert id eff.state s.items}
                       , effects: map (map (AJAXItemAction id)) eff.effects
                       }
  Nothing -> noEffects s
  where
    go { status: AJAXListItem.ItemDeleted} = Nothing
    go itemState = Just (AJAXListItem.update a itemState)


--SO ITS AJAX, commenting ou t refresh button and the other actions fixes it
view :: State -> Html Action
view s = div !
  className "component" ##
  items
    --button ! onClick (const RefreshList) # text "Refresh"
  where
    items = case s.status of
      ListNotFetched -> [text "Not fetched"]
      ListFetching -> [ text "Fetching"
                      , items'
                      ]
      ListFetched -> [items']
      ListFetchError e -> [ text ("Error: " <> e)
                          , items'
                          ]
    items' = ol
      []
      (A.fromFoldable (map viewItem' (M.values s.items)))
    viewItem' i = li #
      map (AJAXItemAction i.id) (AJAXListItem.view i)
