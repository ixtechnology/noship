module Handler.Log where

import Import
import Yesod.Form.Nic        (nicHtmlField)

-- MAYBE BAD
import Data.Maybe            (fromJust)

-- This is a handler function for the GET request method on the LogR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getLogR :: Handler Html
getLogR = do
  muser <- maybeAuth
  entries <- runDB $ selectList [] [Desc EntryPosted]
  (entryWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
            setTitleI MsgBlogArchiveTitle
            $(widgetFile "fancylog")

postLogR :: Handler Html
postLogR = do
  ((res, entryWidget), enctype) <- runFormPost entryForm
  case res of
    FormSuccess entry -> do
             entryId <- runDB $ insert entry
             setMessageI $ MsgEntryCreated $ entryTitle entry
             redirect $ EntryR entryId
    _ -> defaultLayout $ do
             setTitleI MsgPleaseCorrectEntry
             $(widgetFile "fancylogpost")

navbar = $(widgetFile "navbar")

entryForm :: Form Entry
entryForm =renderDivs $ Entry
            <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
            <*> lift (liftIO getCurrentTime)
            <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing


commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
                      <$> pure entryId
                      <*> lift (liftIO getCurrentTime)
                      <*> lift (maybeAuthId >>= return . fromJust) -- ALSO MAYBE BAD
                      <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
                      <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  muser <- maybeAuth
  (entry, comments) <- runDB $ do
                         entry <- get404 entryId
                         comments <- selectList [CommentEntry ==. entryId] [Asc CommentPosted]
                         return (entry, map entityVal comments)
  (commentWidget, enctype) <-
      generateFormPost (commentForm entryId)
  defaultLayout $ do
    setTitleI $ MsgEntryTitle $ entryTitle entry
    $(widgetFile "entry")

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
  ((res, commentWidget), enctype) <-
      runFormPost (commentForm entryId)
  case res of
    FormSuccess comment -> do
                       _ <- runDB $ insert comment
                       setMessageI MsgCommentAdded
                       redirect $ EntryR entryId
    _ -> defaultLayout $ do
                       setTitleI MsgPleaseCorrectComment
                       $(widgetFile "entrypost")
