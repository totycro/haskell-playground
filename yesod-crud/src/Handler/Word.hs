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
import qualified Database.Esqueleto            as E


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

getEmptyDomainR :: Handler Value
getEmptyDomainR = do
    emptyDomains <-
        -- TODO: use exists instead of count() == 0
        runDB $ E.select $ E.from $ \(domain `E.LeftOuterJoin` my_word) -> do
            E.on (domain E.^. DomainId E.==. my_word E.^. MyWordDomain)
            E.groupBy (domain E.^. DomainId)
            let wordCount = E.count (my_word E.^. MyWordId)
            E.having $ wordCount E.==. E.val (0 :: Int)
            E.orderBy [E.asc (domain E.^. DomainName)]
            return domain
    returnJson $ entityVal <$> emptyDomains

serializeDomain :: (Domain, [MyWord]) -> Value
serializeDomain (domain, domWords) =
    object ["name" .= domainName domain, "words" .= (myWordText <$> domWords)]
