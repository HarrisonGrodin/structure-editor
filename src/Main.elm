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

type Ty = NumTy | StrTy

tyToString : Ty -> String
tyToString ty =
  case ty of
    NumTy -> "num"
    StrTy -> "str"

type Template a = Num Int | Plus a a | Neg a | Str String | Concat a a
type TemplateHole a = Template (Template a) | Hole (Maybe Ty)

type Ast = Node (TemplateHole Ast)

map : (a -> b) -> TemplateHole a -> TemplateHole b
map f template =
  case template of
    (Template (Num n)) -> Template (Num n)
    (Template (Plus x1 x2)) -> Template (Plus (f x1) (f x2))
    (Template (Neg x)) -> Template (Neg (f x))
    (Template (Str s)) -> Template (Str s)
    Template (Concat x1 x2) -> Template (Concat (f x1) (f x2))
    Hole ty -> Hole ty

fold : (TemplateHole a -> a) -> Ast -> a
fold f (Node template) = f (map (fold f) template)


type alias Model =
  { ast : Ast
  , errors : List String
  }


init : Model
init = { ast = Node (Hole Nothing), errors = [ "welcome" ] }


-- UPDATE

type Path = PlusL Path | PlusR Path | NegD Path | ConcatL Path | ConcatR Path | Here
type Action = ANum Int | APlus | ANeg | AConcat | AStr String

type Msg = Fill Path Action | Error String


perform : Action -> (Ast, Ty)
perform action =
  case action of
    ANum n  -> (Node (Template (Num n)), NumTy)
    APlus   -> (Node (Template (Plus (Node (Hole (Just NumTy))) (Node (Hole (Just NumTy))))), NumTy)
    ANeg    -> (Node (Template (Neg (Node (Hole (Just NumTy))))), NumTy)
    AStr s  -> (Node (Template (Str s)), StrTy)
    AConcat -> (Node (Template (Concat (Node (Hole (Just StrTy))) (Node (Hole (Just StrTy))))), StrTy)



bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind x f = Maybe.andThen f x

beneath : Maybe Ty -> Ty -> Bool
beneath tau1 tau2 =
  case tau1 of
    Nothing -> True
    Just t -> t == tau2



update_ast : Path -> Action -> Ast -> Maybe Ast
update_ast path action ast =
  case path of
    Here ->
      case ast of
        Node (Hole ty) ->
          let
            (new_ast, new_ty) = perform action
          in
          if beneath ty new_ty
            then Just new_ast
            else Nothing
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
    ConcatL p ->
      case ast of
        Node (Template (Concat l r)) ->
          bind (update_ast p action l) (\x -> Just (Node (Template (Concat x r))))
        _ -> Nothing
    ConcatR p ->
      case ast of
        Node (Template (Concat l r)) ->
          bind (update_ast p action r) (\x -> Just (Node (Template (Concat l x))))
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
    "^" -> Fill path AConcat
    _   ->
      case String.toInt s of
        Nothing -> Fill path (AStr s)
        Just n -> Fill path (ANum n)

node : String -> List (Ast, Path -> Path) -> (Path -> Path) -> Html Msg
node root args path =
  div [] [ text root, Html.ul [] (List.map (\(e, pathCons) -> Html.li [] [ pretty_print e (\p -> path (pathCons p)) ]) args) ]

pretty_print : Ast -> (Path -> Path) -> Html Msg
pretty_print (Node ast) path =
  case ast of
    Hole ty ->
      input
        [ placeholder (
            "hole" ++ (
              case ty of
                Nothing -> ""
                Just constraint_ty -> " of type " ++ tyToString constraint_ty
            )
          )
        , onInput (parse (path Here))
        ]
        []
    Template template ->
      let
        (root, args) =
          case template of
            Num n -> (String.fromInt n, [])
            Neg n -> ("Negative", [ (n, NegD) ])
            Plus l r -> ("Plus", [ (l, PlusL), (r, PlusR) ])
            Str s -> (s, [])
            Concat l r -> ("Concat", [ (l, ConcatL), (r, ConcatR) ])
      in
      node root args path


eval : Ast -> Ast
eval = fold (\template ->
    Node (
      case template of
        Hole ty -> Hole ty
        Template (Num _) -> template
        Template (Plus l r) ->
          case (l, r) of
            (Node (Template (Num xl)), Node (Template (Num xr))) -> Template (Num (xl + xr))
            _ -> template
        Template (Neg n) ->
          case n of
            Node (Template (Num xn)) -> Template (Num (-xn))
            _ -> template
        Template (Str _) -> template
        Template (Concat l r) ->
          case (l, r) of
            (Node (Template (Str xl)), Node (Template (Str xr))) -> Template (Str (xl ++ xr))
            _ -> template
    )
  )

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (String.concat model.errors) ]
    , Html.code [] [ pretty_print model.ast (\x -> x) ]
    , div [] [ text "Result: " , Html.code [] [ pretty_print (eval model.ast) (\x -> x) ] ]
    ]
