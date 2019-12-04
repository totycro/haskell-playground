module WC
    ()
where

import           Data.Char                      ( isSpace )


-- NOTE: Inspired by https://chrispenner.ca/posts/wc

data CharType = IsSpace | NotSpace
    deriving Show

data Flux =
    Flux !CharType
         {-# UNPACK #-} !Int
         !CharType
    | Unknown
    deriving Show

instance Semigroup Flux where
    Unknown           <> x                  = x
    x                 <> Unknown            = x
    Flux l n NotSpace <> Flux NotSpace n' r = Flux l (n + n' - 1) r
    Flux l n _        <> Flux _        n' r = Flux l (n + n') r

instance Monoid Flux where
    mempty = Unknown

flux :: Char -> Flux
flux c | isSpace c = Flux IsSpace 0 IsSpace
       | otherwise = Flux NotSpace 1 NotSpace


data Counts =
    Counts { charCount :: {-# UNPACK #-} !Int

           , wordCount ::  !Flux
           , lineCount :: {-# UNPACK #-} !Int
           }
    deriving (Show)

instance Semigroup Counts where
    (Counts cC wC lC) <> (Counts cC' wC' lC') =
        Counts (cC + cC') (wC <> wC') (lC + lC')

instance Monoid Counts where
    mempty = Counts 0 mempty 0

countChar :: Char -> Counts
countChar c = Counts { charCount = 1
                     , wordCount = (flux c)
                     , lineCount = (if (c == '\n') then 1 else 0)
                     }


countOccurrence :: Char -> Text -> Int
countOccurrence needle hayStack = unOC $ foldMap countOne hayStack
    where countOne c = OC $ if c == needle then 1 else 0

countOccurrence2 :: Char -> Text -> Int
countOccurrence2 needle = foldl' (\count char -> count + countOne char) 0
    where countOne c = if c == needle then 1 else 0


cOFile :: Char -> FilePath -> IO Int
cOFile needle fp = countOccurrence needle <$> readFileUtf8 fp

cOFile2 :: Char -> FilePath -> IO Int
cOFile2 needle fp = countOccurrence2 needle <$> readFileUtf8 fp


newtype OccurrenceCount = OC { unOC :: Int }

-- NOTE: This is just Sum
instance Semigroup OccurrenceCount where
    (OC c) <> (OC c') = OC $ c + c'

instance Monoid OccurrenceCount where
    mempty = OC 0


--
