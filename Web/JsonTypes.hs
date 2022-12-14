{-# language DeriveAnyClass #-}

module Web.JsonTypes where

import Web.Types
import Generated.Types
import IHP.Prelude
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm
import Application.Lib.DerivingViaElm ( ElmType(..) )
import Data.Time
import IHP.ModelSupport ( Violation(..) )
import Data.Char (toLower)

-- JSON serializable types and functions
-- -- for exposing IHP data to Elm and JSON responses


deriving instance Generic Violation
deriving instance SOP.Generic Violation
deriving instance SOP.HasDatatypeInfo Violation
deriving instance Aeson.ToJSON Violation
deriving instance Aeson.FromJSON Violation

instance HasElmType Violation where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Violation defaultOptions "Api.Generated.Violation"

instance HasElmDecoder Aeson.Value Violation where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Violation defaultOptions Aeson.defaultOptions "Api.Generated.violationDecoder"

instance HasElmEncoder Aeson.Value Violation where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Violation defaultOptions Aeson.defaultOptions "Api.Generated.violationEncoder"

data UserJSON = UserJSON
    { errors :: ![(Text, Violation)]
    } deriving ( Generic
               , SOP.Generic
               , SOP.HasDatatypeInfo
               )
     deriving ( Aeson.ToJSON
              , Aeson.FromJSON
              , HasElmType
              , HasElmDecoder Aeson.Value
              , HasElmEncoder Aeson.Value)
        via ElmType "Api.Generated.User" UserJSON


userToJSON :: User -> UserJSON
userToJSON user =
    let metaBag = get #meta user
    in UserJSON {
        errors = map fixFieldName $ annotations metaBag
    }

fixFieldName :: (Text, Violation) -> (Text, Violation)
fixFieldName ("passwordHash", violation) = ("password", violation)
fixFieldName other = other
