module ProductSelect exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Models exposing (Product, Shop, formatPriceRange, viewProductPrice)


type alias Model =
  { shop : Maybe Shop
  , input : String
  }


type Msg
  = Input String


init : Maybe Shop -> Model
init shop =
  { shop = shop
  , input = ""
  }


update : Msg -> Model -> Model
update (Input input) model =
  { model
  | input = input
  }


view : (Product -> a) -> (Msg -> a) -> Model -> Html a
view makeAdd makeInternal model =
  Html.div
    [ Attr.class "productSelect" ]
    [ Html.input
      [ Attr.class "productSelectInput"
      , Attr.value model.input
      , Events.onInput (Input >> makeInternal)
      ]
      []
    , Html.div
      [ Attr.class "productSelectList" ]
      (Models.products
      |> filterProducts model.input
      |> List.map ((previewProduct model) >> Html.map makeAdd))
    ]


previewProduct : Model -> Product -> Html Product
previewProduct model product =
  Html.div
    [ Attr.class "productSelectPreview" ]
    [ Html.div
      [ Attr.class "productName" ]
      [ Html.p [] [ Html.text product.name ] ]
    , Html.div
      [ Attr.class "productCost" ]
      [ Html.p [] [ Html.text <| viewProductPrice product model.shop ] ]
    , Html.button
      [ Attr.class "addProduct"
      , Events.onClick product
      ]
      []
    ]


filterProducts : String -> List Product -> List Product
filterProducts input =
  List.filter (shouldTakeProduct input)


shouldTakeProduct : String -> Product -> Bool
shouldTakeProduct input product =
  not (String.isEmpty input) && String.contains (String.toLower input) (String.toLower product.name)
