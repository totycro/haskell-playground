{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Word where

import           Import


getWordR :: Handler Value
getWordR = do
    wordList <- runDB $ selectList [] []
    returnJson $ object ["data" .= (wordList :: [Entity MyWord])]

postWordR :: Handler Value
postWordR = do
    word <- requireCheckJsonBody :: Handler MyWord
    runDB $ insert400_ word
    sendStatusJSON created201 (object ["status" .= "yeah"])

getWordDetailR :: MyWordId -> Handler Value
getWordDetailR wordId = runDB $ get404 wordId >>= returnJson

putWordDetailR :: MyWordId -> Handler Value
putWordDetailR = undefined

deleteWordDetailR :: MyWordId -> Handler Value
deleteWordDetailR = undefined
