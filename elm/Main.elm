module Main exposing (main)

import Api
import Api.Generated
    exposing
        ( FlashMessage(..)
        , NavBarContext
        , User
        , Violation(..)
        , Widget(..)
        , widgetDecoder
        , PrivateGroup
        )
import Browser
import Browser.Navigation
import Color as SvgColor
import Colors
import Date exposing (Date)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode
import Material.Icons.Action exposing (check_circle)
import Material.Icons.Alert exposing (error_outline)
import Material.Icons.Content exposing (add)
import Material.Icons.Maps exposing (directions_run, hotel)
import Svg exposing (Svg)
import Task
import Time
import Urls
import Util exposing (icon)


type alias Model =
    { currentDate : Date
    , monthIndex : Int
    , widget : WidgetModel
    , flashMessage : Maybe FlashMessage
    , showMenu : Bool
    }


type WidgetModel
    = NavBarModel NavBarContext
    | ErrorModel String
    | AboutModel
    | FlashMessageModel FlashMessage
    | LoginModel { email : String, password : String }
    | NewUserModel { email : String, password : String, errors : List ( String, Violation ) }
    | GroupListModel (List PrivateGroup)


initLoginModel =
    { email = "", password = "" }


initNewUserModel =
    { email = "", password = "", errors = [] }


type Msg
    = NoOp
    | ReceiveDate Date
    | CreateUser
    | UserCreated (Result Http.Error User)
    | Login
    | LoggedIn (Result Http.Error ())
    | Logout
    | LoggedOut (Result Http.Error ())
    | SetFlashMessage (Maybe FlashMessage)
    | ToggleMenu
    | UpdateEmail String
    | UpdatePassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceiveDate date ->
            ( { model
                | currentDate = date
              }
            , Cmd.none
            )

        Logout ->
            ( model
            , Api.logout LoggedOut
            )

        LoggedOut (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just }
            , Cmd.none
            )

        LoggedOut (Ok _) ->
            ( model, Browser.Navigation.load Urls.newSession )

        ToggleMenu ->
            ( { model | showMenu = not model.showMenu }
            , Cmd.none
            )

        SetFlashMessage fm ->
            ( { model | flashMessage = fm }
            , Cmd.none
            )

        UpdateEmail newEmail ->
            case model.widget of
                LoginModel data ->
                    ( { model | widget = LoginModel { data | email = newEmail } }
                    , Cmd.none
                    )

                NewUserModel data ->
                    ( { model | widget = NewUserModel { data | email = newEmail } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdatePassword newPassword ->
            case model.widget of
                LoginModel data ->
                    ( { model | widget = LoginModel { data | password = newPassword } }
                    , Cmd.none
                    )

                NewUserModel data ->
                    ( { model | widget = NewUserModel { data | password = newPassword } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Login ->
            case model.widget of
                LoginModel data ->
                    ( model
                    , Api.login data LoggedIn
                    )

                _ ->
                    ( model, Cmd.none )

        LoggedIn (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just }
            , Cmd.none
            )

        LoggedIn (Ok _) ->
            ( { model | flashMessage = SuccessFlashMessage "you're in" |> Just }
            , Browser.Navigation.load Urls.root
            )

        CreateUser ->
            case model.widget of
                NewUserModel { email, password } ->
                    ( model
                    , Api.createUser { email = email, password = password } UserCreated
                    )

                _ ->
                    ( model, Cmd.none )

        UserCreated (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just }
            , Cmd.none
            )

        UserCreated (Ok user) ->
            case user.errors of
                [] ->
                    ( model
                    , Browser.Navigation.load Urls.root
                    )

                _ ->
                    case model.widget of
                        NewUserModel data ->
                            ( { model | widget = NewUserModel { data | errors = user.errors } }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | flashMessage = ErrorFlashMessage "Something unexpected happened. Please refresh the page." |> Just }
                            , Cmd.none
                            )


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        Http.BadBody s ->
            "Error: " ++ s

        Http.Timeout ->
            "Error: Request timed out."

        Http.BadUrl s ->
            "Error: BadUrl " ++ s

        Http.NetworkError ->
            "Error: Internet broke"

        Http.BadStatus i ->
            "Error: BadStatus " ++ String.fromInt i


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        ( bgColor, fontColor ) =
            ( Colors.white, Colors.black )

        options =
            case model.widget of
                NavBarModel _ ->
                    []

                _ ->
                    [ noStaticStyleSheet ]
    in
    layoutWith { options = options }
        [ nunito, Font.size 16, Background.color bgColor ]
    <|
        case model.widget of
            ErrorModel m ->
                paragraph [] [ text m ]

            NavBarModel context ->
                navBar context model.showMenu

            AboutModel ->
                text "The best free and open source secret santa gift exchange manager"

            FlashMessageModel flashMessage ->
                viewFlashMessage flashMessage

            LoginModel data ->
                loginForm data { flashMessage = model.flashMessage }

            NewUserModel data ->
                newUserForm data { flashMessage = model.flashMessage }

            GroupListModel groups ->
               case groups of 
                    [] ->
                        welcomeScreen
                    _ ->
                        text "Wow. Much groups"


nunito : Attribute msg
nunito =
    Font.family
        [ Font.typeface "Nunito"
        , Font.serif
        ]

navBar : NavBarContext -> Bool -> Element Msg
navBar { loggedIn } showMenu =
    let
        menu =
            if showMenu then
                menuOptions loggedIn

            else
                Element.none
    in
    column [ width fill ]
        [ row
            [ width fill
            , padding 16
            , Background.color Colors.indigo
            ]
            [ column
                [ spacing 4
                , centerX
                , width fill
                , paddingEach { top = 0, left = 24, right = 0, bottom = 0 }
                ]
                [ link
                    [ Region.heading 1
                    , Font.size 28
                    , centerX
                    , Font.center
                    , Font.color Colors.white
                    ]
                    { label = text "White Elephantster"
                    , url = Urls.root
                    }
                ]
            , menuButton
            ]
        , menu
        ]


menuButton : Element Msg
menuButton =
    Input.button
        [ Font.size 28
        , Font.color Colors.white
        , alignRight
        , width (px 24)
        ]
        { label = text "⋮"
        , onPress = Just ToggleMenu
        }


menuOptions : Bool -> Element Msg
menuOptions loggedIn =
    let
        logoutButton : Element Msg
        logoutButton =
            case loggedIn of
                True ->
                    Input.button menuItemAttrs
                        { label = text "Logout"
                        , onPress = Just Logout
                        }

                False ->
                    Element.none
    in
    column
        [ Font.size 18
        , width fill
        , Background.color Colors.indigoDarker
        , Font.color Colors.white
        , paddingXY 12 0
        ]
    <|
        borderBetween Colors.white
            [ link menuItemAttrs { url = Urls.about, label = text "About" }
            , logoutButton
            ]


menuItemAttrs : List (Attribute Msg)
menuItemAttrs =
    [ paddingXY 0 24
    , Font.alignRight
    , width fill
    ]


borderBetween : Color -> List (Element msg) -> List (Element msg)
borderBetween color elements =
    case elements of
        [] ->
            []

        [ element ] ->
            [ element ]

        element :: rest ->
            el
                [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
                , width fill
                , Border.color color
                ]
                element
                :: borderBetween color rest


welcomeScreen : Element Msg
welcomeScreen =
    column
        [ paddingEach { left = 32, right = 0, top = 0, bottom = 0 }
        , spacing 12
        ]
        [ paragraph
            []
            [ text  "Welcome to White Elephantster! "
            , text "You are not currently in any groups and as such will receive nothing but coal this year. To rectify this dire situation, use the button below to create one and then invite all your friends to join. Or, maybe one of your friends already made a group. To keep groups private, you can't search for them, so you'll need to go ask your friend for the link."
            ]
        , link 
            [ paddingXY 6 4
            , Font.size 16
            , Background.color Colors.teal
            , Border.rounded 4
            , Border.width 2
            , Border.color Colors.black
            ]
            { url = Urls.newGroup
            , label = text "Create a new group"
            }
        ]

loginForm :
    { email : String, password : String }
    -> { flashMessage : Maybe FlashMessage }
    -> Element Msg
loginForm { email, password } { flashMessage } =
    column
        [ spacing 24
        , paddingEach { left = 32, right = 0, top = 0, bottom = 0 }
        ]
        [ flashMessage |> Maybe.map viewFlashMessage |> Maybe.withDefault Element.none
        , Input.email
            []
            { label = Input.labelAbove [] <| text "Email"
            , text = email
            , placeholder = Nothing
            , onChange = UpdateEmail
            }
        , Input.currentPassword
            []
            { label = Input.labelAbove [] <| text "Password"
            , text = password
            , onChange = UpdatePassword
            , placeholder = Nothing
            , show = False
            }
        , Input.button
            [ Border.rounded 4
            , Border.color Colors.black
            , Border.width 1
            , paddingXY 24 12
            , Background.color Colors.indigo
            ]
            { label = text "Login"
            , onPress = Just Login
            }
        , paragraph
            [ Font.size 14 ]
            [ text "New here? "
            , link
                [ Font.underline
                , Font.color Colors.blue
                ]
                { url = Urls.newUser
                , label = text "Create an account "
                }
            , text "or "
            , link
                [ Font.underline
                , Font.color Colors.blue
                ]
                { url = Urls.about
                , label = text "learn more."
                }
            ]
        ]


newUserForm :
    { email : String, password : String, errors : List ( String, Violation ) }
    -> { flashMessage : Maybe FlashMessage }
    -> Element Msg
newUserForm { email, password, errors } { flashMessage } =
    let
        emailError =
            errorMessageForField errors "email"

        passwordError =
            errorMessageForField errors "password"
    in
    column
        [ spacing 24
        , paddingEach { left = 32, right = 0, top = 0, bottom = 0 }
        ]
        [ flashMessage |> Maybe.map viewFlashMessage |> Maybe.withDefault Element.none
        , Input.email
            []
            { label = Input.labelAbove [] <| text "Email"
            , text = email
            , placeholder = Nothing
            , onChange = UpdateEmail
            }
        , formError emailError
        , Input.newPassword
            []
            { label = Input.labelAbove [] <| text "Password"
            , text = password
            , onChange = UpdatePassword
            , placeholder = Nothing
            , show = False
            }
        , formError passwordError
        , Input.button
            [ Border.rounded 4
            , Border.color Colors.black
            , Border.width 1
            , paddingXY 24 12
            , Background.color Colors.indigo
            ]
            { label = text "Create Account"
            , onPress = Just CreateUser
            }
        , column
            [ spacing 14 ]
            [ paragraph
                [ Font.size 14 ]
                [ text "Been here before? "
                , link
                    [ Font.underline
                    , Font.color Colors.blue
                    ]
                    { url = Urls.newSession
                    , label = text "Login."
                    }
                ]
            , paragraph
                [ Font.size 14 ]
                [ text "Confused? "
                , link
                    [ Font.underline
                    , Font.color Colors.blue
                    ]
                    { url = Urls.about
                    , label = text "Learn more."
                    }
                ]
            ]
        ]


errorMessageForField : List ( String, Violation ) -> String -> Maybe String
errorMessageForField errors field =
    errors
        |> List.filter (\( f, violation ) -> f == field)
        |> List.head
        |> Maybe.map
            (\( fieldName, violation ) ->
                case violation of
                    TextViolation { message } ->
                        message

                    HtmlViolation { message } ->
                        message
            )


formError : Maybe String -> Element Msg
formError maybeError =
    case maybeError of
        Nothing ->
            Element.none

        Just message ->
                paragraph
                    [ Font.italic
                    , Font.color Colors.darkRed
                    ]
                    [ text message ]


viewFlashMessage : FlashMessage -> Element Msg
viewFlashMessage flashMessage =
    let
        ( message, color ) =
            case flashMessage of
                SuccessFlashMessage m ->
                    ( m, Colors.success )

                ErrorFlashMessage m ->
                    ( m, Colors.error )

        iconFunction =
            case flashMessage of
                SuccessFlashMessage _ ->
                    check_circle

                ErrorFlashMessage _ ->
                    error_outline
    in
    el
        [ paddingXY 24 12 ]
    <|
        row
            [ Border.width 2
            , Border.color color
            , Border.rounded 6
            , paddingXY 12 6
            , spacing 12
            ]
            [ icon iconFunction color 24
            , paragraph [] [ text message ]
            ]



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        widget =
            initialWidgetModel flags
    in
    ( { widget = initialWidgetModel flags
      , currentDate = Date.fromCalendarDate 1 Time.Nov 1995
      , monthIndex = 0
      , flashMessage = Nothing
      , showMenu = False
      }
    , Date.today |> Task.perform ReceiveDate
    )


initialWidgetModel : Json.Decode.Value -> WidgetModel
initialWidgetModel flags =
    case Json.Decode.decodeValue widgetDecoder flags of
        Ok widget ->
            widgetFlagToModel widget

        Err error ->
            ErrorModel (Json.Decode.errorToString error)


widgetFlagToModel : Widget -> WidgetModel
widgetFlagToModel widget =
    case widget of
        NavBarWidget context ->
            NavBarModel context

        AboutWidget ->
            AboutModel

        FlashMessageWidget flashMessage ->
            FlashMessageModel flashMessage

        LoginWidget ->
            LoginModel initLoginModel

        NewUserWidget ->
            NewUserModel initNewUserModel

        GroupListWidget groups ->
            GroupListModel groups


firstCharToUpper : String -> String
firstCharToUpper s =
    (s |> String.left 1 |> String.toUpper)
        ++ (s |> String.dropLeft 1)
