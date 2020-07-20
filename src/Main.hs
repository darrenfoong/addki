{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import Data.Text (Text)
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

data Language = ZH | KR | FR deriving (Eq, Enum, Bounded)

instance Show Language where
    show ZH = "中文"
    show KR = "한국어"
    show FR = "Français"

data EntryF = EntryF
    { language :: Language
    , word :: Text
    , definition :: Text
    , alternateForm :: Maybe Text
    , additionalInfo :: Maybe Text
    , pronunciation :: Maybe Text
    , context :: Maybe Textarea
    , tags :: Maybe Text -- TODO [Text]
    }
    deriving Show

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Entry
    language String
    word String
    definition String
    alternateForm String Maybe
    additionalInfo String Maybe
    pronunciation String Maybe
    context String Maybe
    tags String Maybe
|]

newtype App = App ConnectionPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/entry EntryR GET POST
/json JsonR GET
/oof NotFoundR GET
|]

instance Yesod App where
    defaultLayout = appLayout
    errorHandler NotFound = redirect NotFoundR
    errorHandler other = defaultErrorHandler other

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

entryForm :: Html -> MForm Handler (FormResult EntryF, Widget)
entryForm = renderBootstrap $ EntryF
    <$> areq (selectField optionsEnum) "Language" Nothing
    <*> areq textField "Word" Nothing
    <*> areq textField "Definition" Nothing
    <*> aopt textField "Alternate form" Nothing
    <*> aopt textField "Additional info" Nothing
    <*> aopt textField "Pronunciation" Nothing
    <*> aopt textareaField "Context" Nothing
    <*> aopt textField "Tags" Nothing

appLayout :: Widget -> Handler Html
appLayout widget = do
    pc <- widgetToPageContent $ do
          widget
          toWidgetHead [hamlet|<meta name="keywords" content="anki">|]
          toWidget [lucius|
          body { font: 1.0rem/1.1 sans-serif; }
          #content { padding: 10px; }
          |]
    withUrlRenderer
          [hamlet|
          $doctype 5
          <html>
            <head>
              <title>#{pageTitle pc}
              ^{pageHead pc}
            <body>
              <div #content>
                ^{pageBody pc}
          |]

getHomeR :: Handler Html
getHomeR = do
    (entryFormWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "addki"
        toWidget [hamlet|<h1>addki|]
        toWidget [hamlet|
        <p><em>addki</em> is a tool to retrieve definitions of foreign words from online dictionaries and convert them into an Anki-importable format.
        <p>But I still can't use line breaks in my source code without affecting the HTML output...
        |]
        [whamlet|
        <form method=post action=@{EntryR} enctype=#{enctype}>
          ^{entryFormWidget}
          <button>Add
        |]

getEntryR :: Handler Html
getEntryR = do
    entries <- runDB $ selectList [] [Desc EntryWord, LimitTo 10]
    defaultLayout $ do
        setTitle "addki"
        toWidget [hamlet|<h1>All entries|]
        [whamlet|
            <ul>
                $forall Entity entryId entry <- entries
                    <li>#{entryWord entry}
        |]

postEntryR :: Handler Html
postEntryR = do
    ((result, entryFormWidget), enctype) <- runFormPost entryForm
    case result of
        FormSuccess entryF -> runDB $ insert $ Entry
                                (show (language entryF))
                                (show (word entryF))
                                (show (definition entryF))
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing
    defaultLayout $ do
        setTitle "addki"
        case result of
            FormSuccess entryF -> [whamlet|<p>#{show entryF}|]
            _ -> [whamlet|
                 <form method=post action=@{EntryR} enctype=#{enctype}>
                    ^{entryFormWidget}
                    <button>Add
                 |]

getJsonR :: HandlerFor App Value
getJsonR  = return $ object ["message" .= "Hello World"]

getNotFoundR :: Handler Html
getNotFoundR = defaultLayout $ do
    setTitle "Not found"
    toWidget [hamlet|<h1>Oof!|]
    toWidget [hamlet|
    <p>We couldn't find your page
    |]

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    warp 3000 $ App pool
