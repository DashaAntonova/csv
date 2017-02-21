{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics
import Data.Foldable (traverse_)
import Data.List
import Data.Time
import Data.Maybe
--import Data.Tuple.HT

data Data_csv = Data_csv String Int deriving (Generic, Show)

data Pomodoros = Pomodoros {
    day :: Day,
    sum :: Int 
} deriving (Show)

instance FromRecord Data_csv
instance ToRecord Data_csv

valuesToList :: Data_csv -> Maybe Pomodoros
valuesToList (Data_csv a b) = case (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime) of
                              Nothing -> Nothing
                              isJust -> Just $ Pomodoros dayFromCsv b
                                        where dayFromCsv = localDay localTime
                                              localTime = zonedTimeToLocalTime (fromJust zonedTime)
                                              zonedTime = (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime)

test :: Data_csv -> Maybe ZonedTime
test (Data_csv a b) = (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime)
                                    
listFromCsv :: BL.ByteString -> IO [Data_csv]
listFromCsv csvData = 
    case decode NoHeader csvData of
        Left err -> fail err
        Right v -> return $ V.toList v

sumPomodoros :: [Maybe Pomodoros] -> Maybe Pomodoros
sumPomodoros array = case head array of
                     Nothing -> Nothing
                     isJust -> Just $ Pomodoros (day $ fromJust (head array)) sumArray
                               where sumArray = foldl (\x (Pomodoros a b) -> x+b) 0 (catMaybes array)

equalPomodoros :: Maybe Pomodoros -> Maybe Pomodoros -> Bool
equalPomodoros Nothing Nothing = True
equalPomodoros (Just (Pomodoros x y)) (Just (Pomodoros a b)) = if x == a then True else False
equalPomodoros _ _ = False

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

main :: IO ()
main = do
    csvData <- BL.readFile "pomodoros.csv"
    list <- fmap (map valuesToList) $ listFromCsv csvData
    --traverse_ print list
    --putStrLn "End1"
    
    let groupList = map sumPomodoros (groupBy equalPomodoros list)
    traverse_ print groupList

    putStrLn "End"