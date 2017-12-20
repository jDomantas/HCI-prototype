module ListUtils exposing (placeAt, updateAt)


placeAt : Int -> a -> List a -> List a
placeAt index item list =
  case (index, list) of
    (0, []) ->
      [item]
    
    (0, x :: xs) ->
      item :: xs

    (a, []) ->
      Debug.crash "can't place item"

    (a, x :: xs) ->
      x :: placeAt (a - 1) item xs


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index f list =
  case (index, list) of
    (_, []) ->
      []

    (0, x :: xs) ->
      f x :: xs

    (a, x :: xs) ->
      x :: updateAt (a - 1) f xs
