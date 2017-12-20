module Map exposing (..)

import Dict
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events as Events
import VirtualDom
import Models exposing (ListProduct, Shop, ShoppingList)


type alias Model =
  { list : ShoppingList
  , shop : Shop
  }


init : ShoppingList -> Shop -> Model
init list shop =
  { list = list
  , shop = shop
  }


view : (Int -> a) -> Model -> Html a
view makeMark model =
  let
    items =
      model.list.products
      |> List.indexedMap (\i p -> viewItem (makeMark i) model.shop p)
      |> List.filterMap identity
  in
    Svg.svg
      [ Attr.class "shopPlan"
      , Attr.viewBox "0 0 480 800"
      ]
      ([ Svg.image
        [ Attr.xlinkHref "resources/plan.png" ]
        []
      ] ++ items)


viewItem : a -> Shop -> ListProduct -> Maybe (Svg a)
viewItem click shop product =
  case (product.taken, Dict.get product.name shop.products) of
    (False, Just info) ->
      Just <|
        Svg.image
          [ Attr.x <| toString <| Tuple.first info.position - 20
          , Attr.y <| toString <| Tuple.second info.position - 20
          , Attr.width "40"
          , Attr.height "40"
          , Attr.xlinkHref product.icon
          , Events.onClick click
          ]
          []
    
    _ ->
      Nothing
