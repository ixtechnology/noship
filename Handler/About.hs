module Handler.About where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

-- Handler for GET requests on the about page
getAboutR :: Handler Html
getAboutR = defaultLayout $ do
             aDomId <- newIdent
             setTitle "About Ix Technology"
             $(widgetFile "about")

navbar = $(widgetFile "navbar")
