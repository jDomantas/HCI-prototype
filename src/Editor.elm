module Editor exposing
  ( Model, Msg, View(Home, AddingProduct, SelectingShop, ViewingMap)
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
import Map
import ListUtils


type Subscreen
  = None
  | AddProduct ProductSelect.Model
  | SelectShop ShopSelect.Model
  | ShopMap Map.Model


type View
  = Home
  | AddingProduct
  | SelectingShop
  | ViewingMap


type Msg
  = Nop
  | SetTitle String
  | AddItem
  | OpenShopSelect
  | OpenShopMap
  | Select ProductSelect.Msg
  | ShopUpdate ShopSelect.Msg
  | AddNewItem Product
  | ChangeShop Shop
  | ToggleItem Int
  | RemoveItem Int


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
  { freshEditor
  | list = list
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

    OpenShopMap ->
      case model.list.shop of
        Just shop ->
          { model
          | subscreen = ShopMap (Map.init model.list shop)
          } ! [ Navigation.newUrl "#map" ]
        
        Nothing ->
          model ! []

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

        newList = let l = model.list in { l | products = updated }

        newSubscreen =
          case model.subscreen of
            ShopMap model ->
              ShopMap { model | list = newList }

            other ->
              other
      in
        { model
        | list = newList
        , subscreen = newSubscreen
        } ! []

    RemoveItem index ->
      { model
      | list = let l = model.list in { l | products = ListUtils.removeAt index l.products }
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

    ViewingMap ->
      model


view : a -> (Msg -> a) -> Model -> Html a
view removeMsg makeInternal model =
  case model.subscreen of
    None ->
      Html.div
        [ Attr.class "editor" ]
        (List.map (Html.map makeInternal)
          ([ viewTitle model.list.title
          , Html.div
            [ Attr.class "editorItemList" ]
            (List.indexedMap (viewItem model) model.list.products)
          , newItemButton
          ] ++ (viewShop model)) ++
        [ Html.button
          [ Attr.class "removeList"
          , Events.onClick removeMsg
          ]
          [ Html.text "Remove list" ]
        ])

    AddProduct model ->
      ProductSelect.view AddNewItem Select model
      |> Html.map makeInternal

    SelectShop model ->
      ShopSelect.view ChangeShop ShopUpdate model
      |> Html.map makeInternal

    ShopMap model ->
      Map.view ToggleItem model
      |> Html.map makeInternal


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
      , Html.button
        [ Attr.class "removeItem"
        , Events.onClick (RemoveItem index)
        ]
        [ Html.text "x" ]
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
      , Html.button
        [ Attr.class "shopPlanButton"
        , Events.onClick OpenShopMap
        ]
        [ Html.text "Open shop plan" ]
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
