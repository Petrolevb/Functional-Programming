module Handler.Game where

import Import


entryForm :: Form (Int, Int)
entryForm = renderDivs $ (,)
    <$> areq intField "From" Nothing
    <*> areq intField "To" Nothing

getGameR :: Handler RepHtml
getGameR = do
     (gameWidget, enctype) <- generateFormPost entryForm
     defaultLayout $ do
         setTitle "9 men's Morris"
         $(widgetFile "9menMorris")

postGameR :: Handler RepHtml
postGameR =  do
    ((res,gameWidget),enctype) <- runFormPost entryForm
    case res of 
         FormSuccess (resFrom, resTo) -> do 
            let from = resFrom
            let to = resTo
            defaultLayout $ do
                $(widgetFile "9menMorris")
         _ -> defaultLayout $ do
                $(widgetFile "9menMorris")

getBoardR :: Handler ()
getBoardR = do
    sendFile "image/png" "img/board.png"

getRedR :: Handler ()
getRedR = do
    sendFile "image/png" "img/red.png"

getBlackR :: Handler ()
getBlackR = do
    sendFile "image/png" "img/black.png"
