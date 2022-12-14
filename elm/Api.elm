module Api exposing
    ( createUser
    , login
    , logout
    )

import Api.Generated exposing (User, userDecoder)
import Http
import Json.Decode as D
import Json.Encode
import Urls


post_method =
    "POST"


delete_method =
    "DELETE"


logout : (Result Http.Error () -> msg) -> Cmd msg
logout onFinish =
    ihpRequest
        { method = delete_method
        , headers = []
        , url = Urls.deleteSession
        , body = Http.emptyBody
        , expect = Http.expectWhatever onFinish
        }


login : { email : String, password : String } -> (Result Http.Error () -> msg) -> Cmd msg
login credentials onFinish =
    ihpRequest
        { method = post_method
        , headers = []
        , url = Urls.createSession
        , body = credentialsEncoder credentials |> Http.jsonBody
        , expect = expectWhateverWithErrorMessage onFinish
        }


createUser : { email : String, password : String } -> (Result Http.Error User -> msg) -> Cmd msg
createUser credentials onFinish =
    ihpRequest
        { method = post_method
        , headers = []
        , url = Urls.createUser
        , body = credentialsEncoder credentials |> Http.jsonBody
        , expect = Http.expectJson onFinish userDecoder
        }


credentialsEncoder : { email : String, password : String } -> Json.Encode.Value
credentialsEncoder { email, password } =
    Json.Encode.object
        [ ( "email", Json.Encode.string email )
        , ( "password", Json.Encode.string password )
        ]


errorDecoder : D.Decoder String
errorDecoder =
    D.field "errorMessage" D.string


ihpRequest :
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
ihpRequest { method, headers, url, body, expect } =
    Http.request
        { method = method
        , headers =
            [ Http.header "Accept" "application/json" ] ++ headers
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


expectWhateverWithErrorMessage : (Result Http.Error () -> msg) -> Http.Expect msg
expectWhateverWithErrorMessage toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case D.decodeString errorDecoder body of
                        Ok value ->
                            Err (Http.BadBody value)

                        -- TODO: custom error type
                        Err _ ->
                            Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok ()
