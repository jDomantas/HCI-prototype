module Editor exposing
  ( Model, Msg, View(Home, AddingProduct, SelectingShop)
  , freshEditor, listEditor, intoList, update, view, goToView
  )

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Navigation
import Models exposing
  ( ListProduct, Shop, ShoppingList, Product
  , formatPriceRange, calculateShopCost
  , viewProductPrice
  )
import ProductSelect
import ShopSelect
import ListUtils


type Subscreen
  = None
  | AddProduct ProductSelect.Model
  | SelectShop ShopSelect.Model


type View
  = Home
  | AddingProduct
  | SelectingShop


type Msg
  = Nop
  | SetTitle String
  | AddItem
  | OpenShopSelect
  | Select ProductSelect.Msg
  | ShopUpdate ShopSelect.Msg
  | AddNewItem Product
  | ChangeShop Shop
  | ToggleItem Int


type alias Model =
  { list : ShoppingList
  , subscreen : Subscreen
  }


freshEditor : Model
freshEditor =
  { list = Models.emptyList
  , subscreen = None
  }


listEditor : ShoppingList -> Model
listEditor list =
  { list = list
  , subscreen = None
  }


intoList : Model -> ShoppingList
intoList = .list


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      model ! []

    SetTitle title ->
      { model
      | list = let l = model.list in { l | title = title }
      } ! []

    AddItem ->
      { model
      | subscreen = AddProduct (ProductSelect.init model.list.shop)
      } ! [ Navigation.newUrl "#additem" ]

    OpenShopSelect ->
      { model
      | subscreen = SelectShop (ShopSelect.init model.list)
      } ! [ Navigation.newUrl "#selectshop" ]

    Select msg ->
      case model.subscreen of
        AddProduct submodel ->
          { model | subscreen = AddProduct (ProductSelect.update msg submodel) } ! []
      
        _ ->
          model ! []

    AddNewItem product ->
      let
        prod =
          { icon = product.icon
          , name = product.name
          , priceRange = product.priceRange
          , taken = False
          }  
      in
        { model
        | list = let l = model.list in { l | products = l.products ++ [prod] }
        , subscreen = None
        } ! [ Navigation.back 1 ]

    ShopUpdate msg ->
      case model.subscreen of
        SelectShop submodel ->
          { model | subscreen = SelectShop (ShopSelect.update msg submodel) } ! []
      
        _ ->
          model ! []

    ChangeShop shop ->
      { model
      | list = let l = model.list in { l | shop = Just shop }
      , subscreen = None
      } ! [ Navigation.back 1 ]

    ToggleItem index ->
      let
        update item =
          { item
          | taken = not item.taken
          }

        updated = ListUtils.updateAt index update model.list.products
      in
        { model
        | list = let l = model.list in { l | products = updated }
        } ! []


goToView : View -> Model -> Model
goToView view model =
  case view of
    Home ->
      { model
      | subscreen = None
      }

    AddingProduct ->
      model

    SelectingShop ->
      model


view : Model -> Html Msg
view model =
  case model.subscreen of
    None ->
      Html.div
        [ Attr.class "editor" ]
        ([ viewTitle model.list.title
        , Html.div
          [ Attr.class "editorItemList" ]
          (List.indexedMap (viewItem model) model.list.products)
        , newItemButton
        ] ++ viewShop model)

    AddProduct model ->
      ProductSelect.view AddNewItem Select model

    SelectShop model ->
      ShopSelect.view ChangeShop ShopUpdate model


viewTitle : String -> Html Msg
viewTitle title =
  Html.input
    [ Attr.class "editorTitle"
    , Attr.value title
    , Events.onInput SetTitle
    ]
    []


viewItem : Model -> Int -> ListProduct -> Html Msg
viewItem model index product =
  let
    classes =
      if product.taken then
        [ Attr.class "productView"
        , Attr.class "taken"
        ]
      else
        [ Attr.class "productView" ]
  in
    Html.div
      classes
      [ Html.div
        [ Attr.class "productName" ]
        [ Html.p [] [ Html.text product.name ] ]
      , Html.div
        [ Attr.class "productCost" ]
        [ Html.p [] [ Html.text <| viewProductPrice product model.list.shop ] ]
      , Html.input
        [ Attr.type_ "checkbox"
        , Attr.class "toggleTaken"
        , Attr.checked product.taken
        , Events.onCheck (always (ToggleItem index))
        ]
        []
      ]


newItemButton : Html Msg
newItemButton =
  Html.button
    [ Attr.class "addItem"
    , Events.onClick AddItem
    ]
    [ Html.text "Add Item" ]


viewShop : Model -> List (Html Msg)
viewShop model =
  case model.list.shop of
    Just shop ->
      [ previewShop model.list shop
      , Html.button
        [ Attr.class "selectShopButton"
        , Events.onClick OpenShopSelect
        ]
        [ Html.text "Change shop" ]
      ]
    
    Nothing ->
      [ Html.button
        [ Attr.class "selectShopButton"
        , Events.onClick OpenShopSelect
        ]
        [ Html.text "Select shop" ]
      ]


previewShop : ShoppingList -> Shop -> Html a
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
      [ Html.p [] [ Html.text <| displayCost <| calculateShopCost shop list]]
    ]


displayCost : Models.ShopCost -> String
displayCost cost =
  case cost of
    Models.Price price ->
      "Total price: " ++ toString price ++ " Eur"

    Models.Missing amount ->
      if amount % 10 == 1 && amount % 100 /= 11 then
        toString amount ++ " item missing"
      else
        toString amount ++ " items missing"