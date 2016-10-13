module Components.AJAXListItem
    ( State(..)
    , RawState(..)
    , fromRawState
    , Status(..)
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Argonaut ((.?), decodeJson, class DecodeJson)
import Data.Maybe (Maybe(Nothing))
import Prelude (const, pure, bind)
import Pux.Html (div, (##), (#), (!), button, text, Html)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------


type State = {
      text :: String
    , newText :: Maybe String
    , status :: Status
    , id :: Int
    }

newtype RawState = RawState {
      text :: String
    , id :: Int
    }


data Action = DeleteAJAXItem


instance decodeJsonRawAJAXListItem :: DecodeJson RawState where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "text"
    i <- obj .? "id"
    pure (RawState {text: t, id: i})


fromRawState :: RawState -> State
fromRawState (RawState i) = {
      text: i.text
    , newText: Nothing
    , status: ItemCreated
    , id: i.id
    }


data Status = ItemCreated
            | ItemDeleting
            | ItemDeleted



--TODO: ajax to actually delete from server
update :: Action -> State -> State
update DeleteAJAXItem i = i { status = ItemDeleting }



--TODO: more detail
view :: State -> Html Action
view { status: ItemDeleting } = text "Deleting..."
view i = div ##
  [ text i.text
  , button !
      onClick (const DeleteAJAXItem) #
      text "Delete"
  ]
