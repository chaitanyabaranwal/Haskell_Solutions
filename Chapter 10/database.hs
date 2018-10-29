import Data.Time

data DatabaseItem = DbString String
                | DbNumber Integer
                | DbDate UTCTime
                deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 9003
    , DbString "Hello, World!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [item | (DbDate item) <- db]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [item | (DbNumber item) <- db]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral $ div (sumDb db) (fromIntegral $ length (filterDbNumber db))