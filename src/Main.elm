import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Navigation exposing (Location)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Models
import Editor
import ListView
import ListUtils


type alias Model =
  { lists : List Models.ShoppingList
  , editor : Maybe (Editor.Model, Int)
  }


type Msg
  = EditorMsg Editor.Msg
  | NewList
  | OpenList Models.ShoppingList Int
  | ChangeUrl Location
  | RemoveList Int


type View
  = Home
  | Editor Editor.View


main : Program Never Model Msg
main = Navigation.program ChangeUrl
  { init = init
  , update = update
  , view = view
  , subscriptions = always Sub.none
  }


init : Location -> (Model, Cmd Msg)
init location =
  { lists = []
  , editor = Nothing
  } ! []


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditorMsg msg ->
      case model.editor of
        Just (editor, index) ->
          let
            (newEditor, cmds) = Editor.update msg editor
          in
            { model
            | editor = Just (newEditor, index)
            } ! [Cmd.map EditorMsg cmds]
        
        Nothing ->
          model ! []

    NewList ->
      let
        editingState =
          ( Editor.freshEditor
          , List.length model.lists
          )
      in
        { model
        | editor = Just editingState
        } ! [ Navigation.newUrl "#editor" ]

    OpenList list index ->
      let
        editingState =
          ( Editor.listEditor list
          , index
          )
      in
        { model
        | editor = Just editingState
        } ! [ Navigation.newUrl "#editor" ]

    ChangeUrl newLocation ->
      goToView (parseView newLocation) model ! []

    RemoveList index ->
      { model
      | editor = Nothing
      , lists = ListUtils.removeAt index model.lists
      } ! [ Navigation.back 1 ]


goToView : View -> Model -> Model
goToView view model =
  case view of
    Home ->
      case model.editor of
        Just (editor, index) ->
          let
            lists =
              model.lists
              |> ListUtils.placeAt index (Editor.intoList editor)
          in
            { model
            | lists = lists
            , editor = Nothing
            }

        Nothing ->
          model

    Editor view ->
      case model.editor of
        Just (editor, index) ->
          let
            editor_ = Editor.goToView view editor
          in
            { model
            | editor = Just (editor_, index)
            }

        Nothing ->
          model


view : Model -> Html Msg
view model =
  case model.editor of
    Just (editor, index) ->
      Editor.view (RemoveList index) EditorMsg editor

    Nothing ->
      viewMainScreen model


viewMainScreen : Model -> Html Msg
viewMainScreen model =
  Html.div []
    [ Html.div
      [ Attr.class "mainLists" ]
      (List.indexedMap (\index item -> ListView.view (OpenList item index) item) model.lists)
    , Html.button
      [ Attr.class "newListButton"
      , Events.onClick NewList
      ]
      [ Html.text "Naujas sąrašas" ]
    ]


parseView : Location -> View
parseView location =
  case location.hash of
    "" -> Home
    "#editor" -> Editor Editor.Home
    "#additem" -> Editor Editor.AddingProduct
    "#selectshop" -> Editor Editor.SelectingShop
    "#map" -> Editor Editor.ViewingMap
    _ -> Debug.crash "bad url"
