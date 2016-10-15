module Resources.AJAXList
    ( -- * Types
      ItemId
    , RawListItem(..)
    -- * APIs
    , getList
    , deleteItem
    , updateItem
    ) where


-------------------------------------------------------------------------------
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut (encodeJson, (~>), (:=), class EncodeJson, (.?), decodeJson, class DecodeJson)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Monoid ((<>))
import Network.HTTP.Affjax (put_, delete_, get, AJAX)
import Prelude (Unit, show, map, pure, bind, (<<<))
-------------------------------------------------------------------------------

type ItemId = Int

newtype RawListItem = RawListItem {
      text :: String
    , id :: Int
    }



instance decodeJsonAJAXListItemRawListItem :: DecodeJson RawListItem where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .? "text"
    i <- obj .? "id"
    pure (RawListItem {text: t, id: i})

instance encodeJsonAJAXListItemRawstate :: EncodeJson RawListItem where
  encodeJson (RawListItem s) = "text" := s.text
                            ~> "id" := s.id


-------------------------------------------------------------------------------
getList :: forall eff. Aff ( ajax :: AJAX | eff) (Either String (Array RawListItem))
getList = do
  res <- attempt (get "/items.json")
  let decode reply = decodeJson reply.response
  pure (either (Left <<< show)
               decode
               res)


-------------------------------------------------------------------------------
deleteItem :: forall eff. RawListItem -> Aff ( ajax :: AJAX | eff) (Either String Unit)
deleteItem (RawListItem i) =
  map (bimap show _.response) (attempt (delete_ ("/items/" <> show i.id <> ".json")))


-------------------------------------------------------------------------------
updateItem :: forall eff. RawListItem -> Aff ( ajax :: AJAX | eff) (Either String Unit)
updateItem r@(RawListItem i) =
  map (bimap show _.response) (attempt (put_ ("/items/" <> show i.id <> ".json") (encodeJson r)))
