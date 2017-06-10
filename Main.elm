module Main exposing (..)

import Html exposing (..)
import List exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Date exposing (..)
import Task exposing (..)
import Time exposing (..)


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { day : Maybe Int
    , month : Maybe Int
    , year : Maybe Int
    , startupTime : Time
    }


model : Model
model =
    { day = Nothing
    , month = Nothing
    , year = Nothing
    , startupTime = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Task.perform SetTime Time.now )



-- UPDATE


type Msg
    = Day String
    | Month String
    | Year String
    | SetTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Day day ->
            ( { model | day = Result.toMaybe (String.toInt day) }, Cmd.none )

        Month month ->
            ( { model | month = Result.toMaybe (String.toInt month) }, Cmd.none )

        Year year ->
            ( { model | year = Result.toMaybe (String.toInt year) }, Cmd.none )

        SetTime time ->
            ( { model | startupTime = time }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Here's where you are in your life" ]
        , select [ onInput Day ]
            (createDropdown (List.range 1 31))
        , select [ onInput Month ]
            (createDropdown (List.range 1 12))
        , select [ onInput Year ]
            (createDropdown (List.range 1970 2017))
        , drawWeeksFromModel model
        , p [] [ text "based on average life expectancy in Europe that's ~ 80 years" ]
        , p []
            [ text "inspired by this awesome article: "
            , a [ href "http://waitbutwhy.com/2014/05/life-weeks.html" ] [ text "Your Life in Weeks" ]
            ]
        ]


drawWeeks :
    { day : Int
    , month : Int
    , year : Int
    , startupTime : Time
    }
    -> Html Msg
drawWeeks { day, month, year, startupTime } =
    let
        birthDate =
            case Date.fromString (toString year ++ "-" ++ padZero month ++ "-" ++ padZero day) of
                Err time ->
                    model.startupTime

                Ok time ->
                    Date.toTime time

        lifeLength =
            startupTime - birthDate

        lifeLengthToWeeks =
            floor ((lifeLength / 1000) / 604800)
    in
        div []
            (List.map
                (\el ->
                    if (el < lifeLengthToWeeks) then
                        span [] [ text "x " ]
                    else if (el == lifeLengthToWeeks) then
                        span [] [ text " NOW " ]
                    else
                        span [] [ text "o " ]
                )
                (List.range 1 (52 * 80))
            )


drawWeeksFromModel : Model -> Html Msg
drawWeeksFromModel model =
    case ( model.day, model.month, model.year, model.startupTime ) of
        ( Just day, Just month, Just year, startupTime ) ->
            drawWeeks { day = day, month = month, year = year, startupTime = startupTime }

        _ ->
            text "fill out your date of your birth to see the result"


createDropdown items =
    List.append
        ([ option [] [ text " " ] ])
        (List.map createOption items)


createOption item =
    option [] [ text (padZero item) ]


padZero item =
    if item < 10 then
        "0" ++ toString item
    else
        toString item



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
