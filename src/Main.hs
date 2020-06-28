{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import Yesod
import Data.Text (Text, pack)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/entry/#Text EntryR GET
/json JsonR GET
/oof NotFoundR GET
|]

instance Yesod App where
    defaultLayout = appLayout
    errorHandler NotFound = redirect NotFoundR
    errorHandler other = defaultErrorHandler other

data Entry = Entry
    { language :: Text
    , word :: Text
    , definition :: Text
    , alternateForm :: Maybe Text
    , additionalInfo :: Maybe Text
    , pronunciation :: Maybe Text
    , context :: [Text]
    , tags :: [Text]
    }
    deriving Show

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

getHomeR = defaultLayout $ do
    setTitle "addki"
    toWidget [hamlet|<h1>addki|]
    toWidget [hamlet|
    <p><em>addki</em> is a tool to retrieve definitions of foreign words from online dictionaries and convert them into an Anki-importable format.
    <p>But I still can't use line breaks in my source code without affecting the HTML output...
    |]

getEntryR entry = defaultLayout $ do
    $logInfo $ "Adding: " <> pack (show entry)
    setTitle "addki"
    toWidget [hamlet|
    <p>Adding entry for <em>#{entry}</em>...
    |]

getJsonR  = return $ object ["message" .= "Hello World"]

getNotFoundR = defaultLayout $ do
    setTitle "Not found"
    toWidget [hamlet|<h1>Oof!|]
    toWidget [hamlet|
    <p>We couldn't find your page
    |]

main :: IO ()
main = warp 3000 App
