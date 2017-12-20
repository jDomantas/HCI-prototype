module Models exposing
  ( ListProduct, Product, ProductInfo, ShoppingList, Shop, ShopCost(Price, Missing)
  , emptyList
  , formatPriceRange, calculateShopCost
  , viewProductPrice
  , products, productDict, shops
  )

import Dict exposing (Dict)


type alias Product =
  { priceRange : (Float, Float)
  , name : String
  , icon : String
  }


type alias Markable a = { a | taken : Bool }


type alias ListProduct = Markable Product


type alias ShoppingList =
  { title : String
  , products : List ListProduct
  , shop : Maybe Shop
  }


type alias ProductInfo =
  { price : Float
  , position : (Float, Float)
  }


type alias Shop =
  { name : String
  , address : String
  , products : Dict String ProductInfo
  }


emptyList : ShoppingList
emptyList =
  { title = "New list"
  , products = []
  , shop = Nothing
  }


formatPriceRange : (Float, Float) -> String
formatPriceRange (from, to) =
  if from == to then
    toString from ++ " Eur"
  else
    toString from ++ " - " ++ toString to ++ " Eur"


viewProductPrice : { a | name : String, priceRange : (Float, Float) } -> Maybe Shop -> String
viewProductPrice product shop =
  case shop of
    Just shop ->
      case Dict.get product.name shop.products of
        Just info ->
          formatPriceRange (info.price, info.price)

        Nothing ->
          "Unavailable"

    Nothing ->
      formatPriceRange product.priceRange


type ShopCost
  = Price Float
  | Missing Int


calculateShopCost : Shop -> ShoppingList -> ShopCost
calculateShopCost shop list =
  let
    add : ListProduct -> ShopCost -> ShopCost
    add product total =
      case (Dict.get product.name shop.products, total) of
        (Just info, Price total) ->
          Price (info.price + total)

        (Just _, Missing amount) ->
          Missing amount

        (Nothing, Price _) ->
          Missing 1

        (Nothing, Missing amount) ->
          Missing (amount + 1)
  in
    List.foldl add (Price 0) list.products


rawProducts : List { name : String, icon : String}
rawProducts =
  [ { name = "Thing"
    , icon = ""
    }
  , { name = "Another"
    , icon = ""
    }
  ]


products : List Product
products =
  rawProducts
  |> List.filterMap (\p ->
    let
      shopPrice : Shop -> Maybe Float
      shopPrice shop =
        shop.products
        |> Dict.get p.name
        |> Maybe.map .price

      prices : List Float
      prices =
        List.filterMap shopPrice shops

      lowest = List.minimum prices
      highest = List.maximum prices
    in
      case (lowest, highest) of
        (Just low, Just high) ->
          Just
            { priceRange = (low, high)
            , name = p.name
            , icon = p.icon
            }

        _ ->
          Nothing)


productDict : Dict String Product
productDict =
  products
  |> List.map (\p -> (p.name, p))
  |> Dict.fromList


shops : List Shop
shops =
  let
    makeProduct : { name : String, price : Float } -> (String, ProductInfo)
    makeProduct { name, price } =
      ( name
      , { price = price
        , position = (0, 0)
        }
      )

    makeShop : String -> String -> List { name : String, price : Float } -> Shop
    makeShop name address products =
      { name = name
      , address = address
      , products = Dict.fromList (List.map makeProduct products)
      }
  in
    [ makeShop "Mindaugo maxima" "Mindaugo g. 11"
      [ { name = "Thing", price = 1.5 }
      , { name = "Another", price = 2.3 }
      ]
    , makeShop "Didlaukio IKI" "Didlaukio g. 80A"
      [ { name = "Another", price = 1.8 }
      ]
    ]
