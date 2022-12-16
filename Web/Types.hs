module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import IHP.LoginSupport.Types
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data HomeController
    = HomeAction
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

data UserController
    = NewUserAction
    | CreateUserAction
    deriving (Eq, Show, Data)

data AboutController
    = AboutAction
    deriving (Eq, Show, Data)


data ElephantsterGroupsController
    = ElephantsterGroupsAction
    | NewElephantsterGroupAction
    | ShowElephantsterGroupAction { elephantsterGroupId :: !(Id ElephantsterGroup) }
    | CreateElephantsterGroupAction
    | EditElephantsterGroupAction { elephantsterGroupId :: !(Id ElephantsterGroup) }
    | UpdateElephantsterGroupAction { elephantsterGroupId :: !(Id ElephantsterGroup) }
    | DeleteElephantsterGroupAction { elephantsterGroupId :: !(Id ElephantsterGroup) }
    deriving (Eq, Show, Data)
