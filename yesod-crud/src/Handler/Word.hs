{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Word where

import           Import
import qualified Data.Map                      as Map


-- TODO: call another service, possibly using servant server/client or hreq request library (possibly consider serv)


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

-- | Lists domains together with their words
getDomainR :: Handler Value
getDomainR = do
    domains  <- runDB $ selectList [] [Asc DomainName]
    allWords <- runDB $ selectList [] [Asc MyWordText]
    let wordsMap :: Map (Key Domain) [MyWord] = Map.fromListWith
            (++)
            [ (myWordDomain $ entityVal word, [entityVal word])
            | word <- allWords
            ]
    let domainsWithWords :: [(Domain, [MyWord])] =
            [ (entityVal dom, findWithDefault [] (entityKey dom) wordsMap)
            | dom <- domains
            ]
    returnJson $ Array $ fromList $ serializeDomain <$> domainsWithWords

serializeDomain :: (Domain, [MyWord]) -> Value
serializeDomain (domain, domWords) =
    object ["name" .= domainName domain, "words" .= (myWordText <$> domWords)]
