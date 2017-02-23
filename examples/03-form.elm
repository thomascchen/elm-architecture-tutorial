import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex
import String exposing (..)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { age: String
  , name : String
  , password : String
  , passwordAgain : String
  }


model : Model
model =
  Model "" "" "" ""



-- UPDATE


type Msg
    = Age String
    | Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Age age ->
      { model | age = age }

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
    , viewInput "age" "Age" Age
    , viewValidation model
    ]

viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput inputType placeholderText updateFunction =
    input [ type_ inputType, placeholder placeholderText, onInput updateFunction ] []

viewValidation : Model -> Html Msg
viewValidation model =
  let
    (color, message) =
      if (model.password == model.passwordAgain)
        && (length model.password >= 8)
        && (Regex.contains (Regex.regex "[a-z]") model.password == True )
        && (Regex.contains (Regex.regex "[A-Z]") model.password == True )
        && (Regex.contains (Regex.regex "[0-9]") model.password == True )
        && (Regex.contains (Regex.regex "^[0-9]*$") model.age == True ) then
          ("green", "OK")
      else if (model.password /= model.passwordAgain) then
        ("red", "Passwords do not match!")
      else if (length model.password < 8) then
        ("red", "Password needs to be at least 8 characters!")
      else if (Regex.contains (Regex.regex "[a-z]") model.password == False) then
        ("red", "Password must contain lower case characters")
      else if (Regex.contains (Regex.regex "[A-Z]") model.password == False) then
        ("red", "Password must contain upper case characters")
      else if (Regex.contains (Regex.regex "^[0-9]*$") model.age == False) then
        ("red", "Age must be a number")
      else
        ("red", "Password must container numeric characters")
  in
    div [ style [("color", color)] ] [ text message ]
