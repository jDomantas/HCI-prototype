module ShopSelect exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Models exposing
  ( Shop, ShoppingList
  , calculateShopCost, formatPriceRange
  )


type alias Model =
  { list : ShoppingList
  , input : String
  }


type Msg
  = Input String


init : ShoppingList -> Model
init list =
  { list = list
  , input = ""
  }


update : Msg -> Model -> Model
update (Input input) model =
  { model
  | input = input
  }


view : (Shop -> a) -> (Msg -> a) -> Model -> Html a
view makeAdd makeInternal model =
  Html.div
    [ Attr.class "shopSelect" ]
    [ Html.input
      [ Attr.class "shopSelectInput"
      , Attr.value model.input
      , Events.onInput (Input >> makeInternal)
      ]
      []
    , Html.div
      [ Attr.class "shopSelectList" ]
      (selectViewedShops model.input
      |> List.map (previewShop model.list >> Html.map makeAdd))
    , Html.div
      [ Attr.class "suggestedShopsTitle" ]
      [ Html.p [] [ Html.text "Siūlomos parduotuvės" ] ]
    , Html.div
      [ Attr.class "shopSelectList"
      , Attr.class "suggestedShopList"
      ]
      (selectSuggestedShops model.input
      |> List.map (previewShop model.list >> Html.map makeAdd))
    ]


previewShop : ShoppingList -> Shop -> Html Shop
previewShop list shop =
  Html.div
    [ Attr.class "shopSelectPreview" ]
    [ Html.div
      [ Attr.class "shopName" ]
      [ Html.p [] [ Html.text shop.name ] ]
    , Html.div
      [ Attr.class "shopAddress" ]
      [ Html.p [] [ Html.text shop.address ] ]
    , Html.div
      [ Attr.class "shopCost" ]
      [ Html.p [] [ Html.text <| displayCost <| calculateShopCost shop list ] ]
    , Html.button
      [ Attr.class "pickShop"
      , Events.onClick shop
      ]
      [ Html.text "+" ]
    , Html.img
      [ Attr.class "shopIcon"
      , Attr.src shop.icon
      ]
      []
    ]


selectViewedShops : String -> List Shop
selectViewedShops input =
  if String.isEmpty input then
    []
  else
    List.filter (shouldTakeShop input) Models.shops


selectSuggestedShops : String -> List Shop
selectSuggestedShops input =
  List.take 1 Models.shops


shouldTakeShop : String -> Shop -> Bool
shouldTakeShop input shop =
  let
    low = String.toLower input
    name = String.toLower shop.name
    address = String.toLower shop.address
  in
    String.contains input name || String.contains input address


displayCost : Models.ShopCost -> String
displayCost cost =
  case cost of
    Models.Price price ->
      "Bendra kaina: " ++ formatPriceRange (price, price)

    Models.Missing amount ->
      if amount % 10 == 1 && amount % 100 /= 11 then
        "Neturi " ++ toString amount ++ " prekės"
      else
        "Neturi " ++ toString amount ++ " prekių"
