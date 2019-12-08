{-# LANGUAGE OverloadedStrings #-}
module MonadTransformerExercises
    ( a
    , aTime
    , runIO
    )
where

-- Solutions to
-- https://proglang.informatik.uni-freiburg.de/teaching/functional-programming/2017/exercises/ex7.pdf

import           Prelude                 hiding ( log )

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer

import           Data.Time.Clock.POSIX          ( getPOSIXTime
                                                , POSIXTime
                                                )


import           Control.Concurrent             ( threadDelay )



-- ========================================
-- = 1. Access data (MaybeT, Reader)
-- ========================================

data ProtectedData a = ProtectedData String a


accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pw v) = if s == pw then Just v else Nothing


type Protected s a = MaybeT (Reader (ProtectedData s)) a

run :: ProtectedData s -> Protected s a -> Maybe a
run pd p = runIdentity $ runReaderT (runMaybeT p) pd

access :: String -> Protected a a
access pw = do
    protectedData <- lift ask
    MaybeT $ pure (accessData pw protectedData)



accessSucc = run (ProtectedData "foo" 3) (access "foo")
accessErr = run (ProtectedData "foo" 3) (access "wrong")



type ProtectedIO s a = MaybeT (ReaderT (ProtectedData s) IO) a

runIO :: ProtectedData s -> ProtectedIO s a -> IO (Maybe a)
runIO pd pio = runReaderT (runMaybeT pio) pd

accessIO :: ProtectedIO a a
accessIO = do
    pd <- lift ask
    liftIO $ putStrLn "yo?"
    inputPw <- unpack <$> getLine
    MaybeT $ pure $ accessData inputPw pd




-- ========================================
-- = 2. Log (Writer)
-- ========================================


data Item = Msg String | Section String [Item] deriving (Show,Eq)
type Log = [Item]
type Logging a = Writer Log a

log :: Show t => t -> Logging ()
log el = tell [Msg $ show el]

withSection :: String -> Logging a -> Logging a
withSection sectionName sectionLogging = writer (leData, [section])
  where
    section                   = Section sectionName sectionLogItems
    (leData, sectionLogItems) = runWriter sectionLogging


withSectionNice :: String -> Logging a -> Logging a
withSectionNice sectionName sectionLogging = pass $ do
    leData <- sectionLogging
    return (leData, \items -> [Section sectionName items])


withSectionNice2 :: String -> Logging a -> Logging a
withSectionNice2 sectionName sectionLogging = pass
    (return (leData, const [Section sectionName sectionLogItems]))
    where (leData, sectionLogItems) = runWriter sectionLogging



logger :: Logging Int
logger = do
    let leData = 4
    withSection "foo" $ do
        log ("bar: " <> show leData)
        log "baz"
    withSectionNice "NoValue" (log "no value")
    withSectionNice2 "Stuff" $ do
        log "aItem"
        log "bItem"
        log "cItem"
        return leData


runLogging :: Logging a -> (a, Log)
runLogging = runWriter

a :: (Int, Log)
a = runLogging logger


data ItemTime = MsgTime POSIXTime String | SectionTime String [ItemTime] deriving (Show,Eq)
type LogTime = [ItemTime]
type LoggingTime a = WriterT LogTime IO a

logTime :: Show t => t -> LoggingTime ()
logTime el = do
    time <- lift getPOSIXTime
    tell [MsgTime time (show el)]

withSectionTime :: String -> LoggingTime a -> LoggingTime a
withSectionTime sectionName sectionLogging = pass $ do
    leData <- sectionLogging
    return (leData, \items -> [SectionTime sectionName items])

runLoggingTime :: LoggingTime a -> IO (a, LogTime)
runLoggingTime = runWriterT

adder :: LoggingTime (Int -> Int)
adder = return (+ 1)

loggerTime :: LoggingTime Int
loggerTime = do
    logTime "now"
    lift $ threadDelay $ 3 * 10 ^ 5
    logTime "now2"
    lift $ threadDelay $ 3 * 10 ^ 5
    f <- adder
    lift $ threadDelay $ 3 * 10 ^ 5
    logTime "then"
    return $ f 2

aTime :: IO (Int, LogTime)
aTime = runLoggingTime loggerTime
