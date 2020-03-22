{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/json JsonR GET
|]

instance Yesod App

getHomeR = defaultLayout $ do
    setTitle "addki"
    [whamlet|<h1>addki|]
    [whamlet|
    <p><em>addki</em> is a tool to retrieve definitions of foreign words from online dictionaries and convert them into an Anki-importable format.
    <p>But I still can't use line breaks in my source code without affecting the HTML output...
    |]

getJsonR  = return $ object ["message" .= "Hello World"]

main :: IO ()
main = warp 3000 App
