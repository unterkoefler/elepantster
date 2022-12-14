{-# language DeriveAnyClass #-}

module Application.Helper.View (
    homeWidget,
    navBarWidget,
    aboutWidget,
    flashMessageWidget,
    loginWidget,
    newUserWidget,
    Widget(..),
    NavBarContext(..)
) where

-- Here you can add functions which are available in all your views
import IHP.FlashMessages.Types
import IHP.ViewPrelude
import Generated.Types
import Data.Aeson as Aeson
import Web.JsonTypes
import qualified Generics.SOP as SOP
import GHC.Generics
import Language.Haskell.To.Elm
import Application.Lib.DerivingViaElm ( ElmType(..) )
import Web.Types

data Widget
    = NavBarWidget NavBarContext
    | AboutWidget
    | HomeWidget
    | LoginWidget
    | NewUserWidget
    | FlashMessageWidget FlashMessage
    deriving ( Generic
             , Aeson.ToJSON
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )

deriving instance Generic FlashMessage
deriving instance Aeson.ToJSON FlashMessage
deriving instance SOP.Generic FlashMessage
deriving instance SOP.HasDatatypeInfo FlashMessage
instance HasElmType FlashMessage where
    elmDefinition = Just $ "Api.Generated.FlashMessage"
                            |> deriveElmTypeDefinition @FlashMessage
                                Language.Haskell.To.Elm.defaultOptions
instance HasElmDecoder Aeson.Value FlashMessage  where
  elmDecoderDefinition =
    Just $ "Api.Generated.flashMessageDecoder"
              |> deriveElmJSONDecoder @FlashMessage
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions

instance HasElmEncoder Aeson.Value FlashMessage where
  elmEncoderDefinition =
    Just $ "Api.Generated.flashMessageEncoder"
              |> deriveElmJSONEncoder @FlashMessage
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions


-- haskell-to-elm instances for the Widget type

instance HasElmType Widget where
  elmDefinition =
    Just $ "Api.Generated.Widget"
              |> deriveElmTypeDefinition @Widget
                Language.Haskell.To.Elm.defaultOptions

instance HasElmDecoder Aeson.Value Widget where
  elmDecoderDefinition =
    Just $ "Api.Generated.widgetDecoder"
              |> deriveElmJSONDecoder @Widget
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions

instance HasElmEncoder Aeson.Value Widget where
  elmEncoderDefinition =
    Just $ "Api.Generated.widgetEncoder"
              |> deriveElmJSONEncoder @Widget
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions


data NavBarContext = NavBarContext
    { loggedIn :: Bool
    } deriving ( Generic
               , SOP.Generic
               , SOP.HasDatatypeInfo
               )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
        via ElmType "Api.Generated.NavBarContext" NavBarContext

-- Widgets

navBarWidget :: NavBarContext -> Html
navBarWidget =
    widgetToHtml . NavBarWidget

aboutWidget :: Html
aboutWidget = widgetToHtml AboutWidget

homeWidget :: Html
homeWidget = widgetToHtml HomeWidget

flashMessageWidget :: FlashMessage -> Html
flashMessageWidget =
    widgetToHtml . FlashMessageWidget

loginWidget :: Html
loginWidget = widgetToHtml LoginWidget

newUserWidget :: Html
newUserWidget = widgetToHtml NewUserWidget

widgetToHtml :: Widget -> Html
widgetToHtml widget = [hsx|
    <div data-flags={encode widget} class="elm"></div>
|]
