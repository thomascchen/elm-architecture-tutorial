import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (..)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


model : Model
model =
  Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" Name
    , viewInput "password" "Password" Password
    , viewInput "password" "Re-enter Password" PasswordAgain
    , viewValidation model
    ]

viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput inputType placeholderText updateFunction =
    input [ type_ inputType, placeholder placeholderText, onInput updateFunction ] []

viewValidation : Model -> Html Msg
viewValidation model =
  let
    (color, message) =
      if (model.password == model.passwordAgain) && (length model.password >= 8) then
        ("green", "OK")
      else if (model.password /= model.passwordAgain) then
        ("red", "Passwords do not match!")
      else
        ("red", "Password needs to be at least 8 characters!")
  in
    div [ style [("color", color)] ] [ text message ]
