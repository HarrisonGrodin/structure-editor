module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Template a = Num Int | Plus a a | Neg a
type TemplateHole a = Template (Template a) | Hole

type Ast = Node (TemplateHole Ast)

map : (a -> b) -> TemplateHole a -> TemplateHole b
map f template =
  case template of
    (Template (Num n)) -> Template (Num n)
    (Template (Plus x1 x2)) -> Template (Plus (f x1) (f x2))
    (Template (Neg x)) -> Template (Neg (f x))
    Hole -> Hole

fold : (TemplateHole a -> a) -> Ast -> a
fold f (Node template) = f (map (fold f) template)


type alias Model =
  { ast : Ast
  , errors : List String
  }


init : Model
init = { ast = Node Hole, errors = [ "welcome" ] }


-- UPDATE

type Path = PlusL Path | PlusR Path | NegD Path | Here
type Action = ANum Int | APlus | ANeg

type Msg = Fill Path Action | Error String


perform : Action -> Ast
perform action =
  case action of
    ANum n -> Node (Template (Num n))
    APlus  -> Node (Template (Plus (Node Hole) (Node Hole)))
    ANeg   -> Node (Template (Neg (Node Hole)))


bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind x f = Maybe.andThen f x


update_ast : Path -> Action -> Ast -> Maybe Ast
update_ast path action ast =
  case path of
    Here ->
      case ast of
        Node Hole -> Just (perform action)
        _ -> Nothing
    PlusL p ->
      case ast of
        Node (Template (Plus l r)) ->
          bind (update_ast p action l) (\x -> Just (Node (Template (Plus x r))))
        _ -> Nothing
    PlusR p ->
      case ast of
        Node (Template (Plus l r)) ->
          bind (update_ast p action r) (\x -> Just (Node (Template (Plus l x))))
        _ -> Nothing
    NegD p ->
      case ast of
        Node (Template (Neg e)) ->
          bind (update_ast p action e) (\x -> Just (Node (Template (Neg x))))
        _ -> Nothing


update : Msg -> Model -> Model
update msg model =
  case msg of
    Fill path action ->
      case update_ast path action model.ast of
        Just ast -> { model | ast = ast }
        Nothing -> { model | errors = "oh no" :: model.errors }
    Error s -> { model | errors = s :: model.errors }



-- VIEW

parse : Path -> String -> Msg
parse path s =
  case s of
    "+" -> Fill path APlus
    "-" -> Fill path ANeg
    _   ->
      case String.toInt s of
        Nothing -> Error "invalid num"
        Just n -> Fill path (ANum n)

node : String -> List (Ast, Path -> Path) -> (Path -> Path) -> Html Msg
node root args path =
  div [] [ text root, Html.ul [] (List.map (\(e, pathCons) -> Html.li [] [ pretty_print e (\p -> path (pathCons p)) ]) args) ]

pretty_print : Ast -> (Path -> Path) -> Html Msg
pretty_print (Node ast) path =
  case ast of
    Hole -> input [ placeholder "hole", onInput (parse (path Here))] []
    Template template ->
      let
        (root, args) =
          case template of
            Num n -> (String.fromInt n, [])
            Neg n -> ("Negative", [ (n, NegD) ])
            Plus l r -> ("Plus", [ (l, PlusL), (r, PlusR) ])
      in
      node root args path


eval : Ast -> Maybe Int
eval = fold  (\template ->
  case template of
    Hole -> Nothing
    Template (Num n) -> Just n
    Template (Plus l r) -> bind l (\xl -> bind r (\xr -> Just (xl + xr)))
    Template (Neg n) -> bind n (\x -> Just (-x))
  )

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (String.concat model.errors) ]
    , Html.code [] [ pretty_print model.ast (\x -> x) ]
    , div [] [ text "Result: " ,
        case eval model.ast of
          Nothing -> text "(holes still present)"
          Just n -> Html.code [] [ text (String.fromInt n) ]
      ]
    ]
