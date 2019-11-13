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
    _       <- runDB $ get404 wordId
    newWord <- requireCheckJsonBody :: Handler MyWord
    runDB $ replace wordId newWord

deleteWordDetailR :: MyWordId -> Handler ()
deleteWordDetailR wId = do
    runDB $ delete wId
    sendStatusJSON status204 ()

getDomainR :: Handler Value
getDomainR = do
    domains          <- runDB $ selectList [] []
    domainsWithWords <- forM domains $ \dom -> do
        -- TODO: more efficient implementation (like select_related?)
        domWords <- runDB $ selectList [MyWordDomain ==. entityKey dom] []
        return (entityVal dom, entityVal <$> domWords)
    returnJson $ Array $ fromList $ serializeDomain <$> domainsWithWords

serializeDomain :: (Domain, [MyWord]) -> Value
serializeDomain (domain, domWords) =
    object ["name" .= domainName domain, "words" .= (myWordText <$> domWords)]
