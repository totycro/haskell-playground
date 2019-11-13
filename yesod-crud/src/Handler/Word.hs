{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Word where

import           Import


update404 wordId d = do
    _ <- get404 wordId
    update wordId d


getWordR :: Handler Value
getWordR = do
    wordList <- runDB $ selectList [] []
    returnJson $ object ["data" .= (wordList :: [Entity MyWord])]

postWordR :: Handler ()
postWordR = do
    word <- requireCheckJsonBody :: Handler MyWord
    runDB $ insert400_ word
    sendStatusJSON created201 ()

getWordDetailR :: MyWordId -> Handler Value
getWordDetailR wordId = runDB $ get404 wordId >>= returnJson

putWordDetailR :: MyWordId -> Handler ()
putWordDetailR wordId = do
    word <- requireCheckJsonBody :: Handler MyWord
    runDB $ update404
        wordId
        [MyWordText =. myWordText word, MyWordDomain =. myWordDomain word]

deleteWordDetailR :: MyWordId -> Handler ()
deleteWordDetailR wId = do
    runDB $ delete wId
    sendStatusJSON status204 ()
