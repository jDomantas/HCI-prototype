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
  , icon : String
  , products : Dict String ProductInfo
  }


emptyList : ShoppingList
emptyList =
  { title = "Sąrašas"
  , products = []
  , shop = Nothing
  }


formatPriceRange : (Float, Float) -> String
formatPriceRange (from, to) =
  let
    pad : Int -> String
    pad num =
      if num < 10 then
        "0" ++ toString num
      else
        toString num

    formatNum : Float -> String
    formatNum x = toString (floor x) ++ "." ++ pad (floor (x * 100) % 100)
  in
    if from == to then
      formatNum from ++ " Eur"
    else
      formatNum from ++ " - " ++ formatNum to ++ " Eur"


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


rawProducts : List { name : String, icon : String}
rawProducts =
  [ { name = "Juoda duona"
    , icon = "https://dtgxwmigmg3gc.cloudfront.net/files/5265dd0ec566d72651002d43-icon-256x256.png"
    }
  , { name = "Batonas"
    , icon = "https://d1u5p3l4wpay3k.cloudfront.net/arksurvivalevolved_gamepedia/9/9f/Baked_Bread_Loaf_%28Primitive_Plus%29.png"
    }
  , { name = "Tuoletinis popierius"
    , icon = "http://icons.iconarchive.com/icons/rade8/body-care/128/toilet-paper-icon.png"
    }
  , { name = "Glock"
    , icon = "https://vignette.wikia.nocookie.net/dayzeromod/images/e/e9/Glock_17.png/revision/latest?cb=20130907173734"
    }
  ]


shops : List Shop
shops =
  let
    makeProduct { name, price, at } =
      ( name
      , { price = price
        , position = at
        }
      )

    makeShop name address icon products =
      { name = name
      , address = address
      , icon = icon
      , products = Dict.fromList (List.map makeProduct products)
      }
  in
    [ makeShop "Stoties IKI" "Sodų g. 22" "https://www.iki.lt/apple-touch-icon.png"
      [ { name = "Juoda duona", price = 1, at = (150, 150) }
      , { name = "Tuoletinis popierius", price = 1.5, at = (310, 550) }
      ]
    , makeShop "Mindaugo maxima" "Mindaugo g. 11" "https://www.maxima.lt/images/front/logos/maxima_logo.png"
      [ { name = "Juoda duona", price = 1, at = (150, 150) }
      , { name = "Batonas", price = 5.3, at = (200, 150) }
      , { name = "Tuoletinis popierius", price = 100, at = (300, 350) }
      ]
    , makeShop "Didlaukio IKI" "Didlaukio g. 80A" "https://www.iki.lt/apple-touch-icon.png"
      [ { name = "Juoda duona", price = 1.1, at = (150, 350) }
      , { name = "Batonas", price = 4.5, at = (200, 350) }
      , { name = "Tuoletinis popierius", price = 1.5, at = (310, 550) }
      , { name = "Glock", price = 0.35, at = (150, 610) }
      ]
    ]
