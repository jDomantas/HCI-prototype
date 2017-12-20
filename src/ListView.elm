module ListView exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Models exposing (ShoppingList, Product, ListProduct)


view : a -> ShoppingList -> Html a
view clickMsg list =
  Html.div
    [ Attr.class "listPreview"
    , Events.onClick clickMsg
    ]
    (listHeader list.title :: listItems list.products)


listHeader : String -> Html a
listHeader title =
  Html.div
    [ Attr.class "listPreviewHeader" ]
    [ Html.p [] [ Html.text title ] ]


listItem : ListProduct -> Html a
listItem product =
  let
    classes =
      if product.taken then
        [ Attr.class "listPreviewItem"
        , Attr.class "strikeout"
        ]
      else
        [ Attr.class "listPreviewItem" ]
  in
    Html.div
      classes
      [ Html.p [] [ Html.text product.name ] ]


listItems : List ListProduct -> List (Html a)
listItems products =
  case products of
    a :: b :: c :: ((_ :: _) as rest) ->
      [ listItem a
      , listItem b
      , listItem c
      , shortenedListHint (List.length rest)
      ]

    short ->
      List.map listItem products


shortenedListHint : Int -> Html a
shortenedListHint amount =
  Html.div
    [ Attr.class "listPreviewEnd" ]
    [ Html.p [] [ Html.text <| "... (" ++ toString amount ++ " paslėptos)" ] ]
