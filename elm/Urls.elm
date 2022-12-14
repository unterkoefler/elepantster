module Urls exposing
    ( about
    , createSession
    , createUser
    , deleteSession
    , newSession
    , newUser
    , root
    )

import Date exposing (Date)
import Url.Builder


root : String
root =
    Url.Builder.absolute [] []


about : String
about =
    Url.Builder.absolute [ "About" ] []

deleteSession : String
deleteSession =
    Url.Builder.absolute [ "DeleteSession" ] []


newSession : String
newSession =
    Url.Builder.absolute [ "NewSession" ] []


createSession : String
createSession =
    Url.Builder.absolute [ "CreateSession" ] []


newUser =
    Url.Builder.absolute [ "NewUser" ] []


createUser =
    Url.Builder.absolute [ "CreateUser" ] []


