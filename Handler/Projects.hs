module Handler.Projects where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

-- Handler for GET requests on the projects page
getProjectsR :: Handler Html
getProjectsR = defaultLayout $ do
                 aDomId <- newIdent
                 setTitle "Ix Technology Projects"
                 $(widgetFile "projects")

navbar = $(widgetFile "navbar")
