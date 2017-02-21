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

{-А вот так делать можно? Я же, по сути, использую эту страшную fromJust, но уверена,
что она не возвращает Nothing...-}
valuesToList :: Data_csv -> Maybe Pomodoros
valuesToList (Data_csv a b) = case (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime) of
                              Nothing -> Nothing
                              isJust -> Just $ Pomodoros dayFromCsv b
                                        where dayFromCsv = localDay localTime
                                              localTime = zonedTimeToLocalTime (zonedTime)
                                              zonedTime = fromJust (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime)

listFromCsv :: BL.ByteString -> IO [Data_csv]
listFromCsv csvData = 
    case decode NoHeader csvData of
        Left err -> fail err
        Right v -> return $ V.toList v

sumPomodoros :: [Pomodoros] -> Pomodoros
sumPomodoros array = Pomodoros (day (head array)) sumArray
                where sumArray = foldl (\x (Pomodoros a b) -> x+b) 0 array

equalPomodoros :: Pomodoros -> Pomodoros -> Bool
equalPomodoros (Pomodoros x y) (Pomodoros a b) = if x == a then True else False

nothing_bool :: [Maybe Pomodoros] -> Bool
nothing_bool [] = False
nothing_bool (x:xs) = if (isNothing x) then True else (nothing_bool xs)

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

main :: IO ()
main = do
    csvData <- BL.readFile "pomodoros.csv"
    list <- fmap (map valuesToList) $ listFromCsv csvData
    
    --Из 7го ишуса - программа должна честно сообщать об ошибках, чтобы пользователь мог принять меры
    if nothing_bool list then putStrLn "function valuesToList - error ZonedTime" else putStr ""
    
    let groupList = map sumPomodoros (groupBy equalPomodoros (catMaybes list))
    traverse_ print groupList

    putStrLn "End"