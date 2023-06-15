

--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- UP954123
--

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Monoid
import Text.Printf
import Control.Concurrent


type North = Float

type East = Float

type SName = String

type Temperatures = Float


data Station = Station
  { name :: SName,
    location :: (North,East),
    temperature :: [Temperatures]
  }
    deriving (Eq,Ord,Show,Read)

months :: [String]
months = ["January","February","March","April","May","June","July","August","September","October","November","December"]    

testData :: [Station]
testData = [ Station "Mumbles Head" (51.565, -3.981) [8.26, 8.33, 9.84, 12.36, 15.24, 17.83, 19.55, 19.67, 17.97, 14.70, 11.49, 9.09], Station "Greenwich Park" (51.477, 0.004) [8.47, 9.21, 12.07, 15.35, 18.59, 21.37, 23.75, 23.31, 20.29, 15.83, 11.55, 8.85], Station "Solent" (50.807, -1.208) [8.56, 8.74, 11.01, 13.94, 17.07, 19.59, 21.62, 21.61, 19.38, 15.73, 11.88, 9.17], Station "Ronaldsway" (54.085, -4.632) [8.47, 8.35, 9.44, 11.48, 14.33, 16.52, 18.19, 18.15, 16.56, 13.83, 11.10, 9.17], Station "Baltasound" (60.749, -0.850)[6.55, 6.32, 7.35, 9.16, 11.20, 13.25, 15.08, 15.39, 13.62, 10.88, 8.47, 7.00], Station "St Austell" (50.337, -4.787) [9.46, 9.65, 11.33, 13.30, 16.18, 18.10, 20.60, 20.36, 18.54, 14.99, 12.30, 10.18],Station "Heathrow" (51.479, -0.449) [8.42, 8.98, 11.73, 15.00, 18.37, 21.57, 23.89, 23.40, 20.22, 15.81, 11.47, 8.79], Station "Hunstanton" (52.939, 0.493)  [7.05, 7.45, 9.77, 12.65, 15.96, 18.84, 21.34, 21.28, 18.32, 14.46, 10.29, 7.56], Station "Durham" (54.767, -1.583) [6.86, 7.75, 9.87, 12.49, 15.42, 17.96, 20.24, 19.87, 17.36, 13.51, 9.65, 7.07],  Station "Monks Wood" (52.400, -0.233) [7.58, 8.36, 11.05, 14.14, 17.19, 20.01, 22.63, 22.49, 19.50, 15.18, 10.68, 7.85]]


--Demo 1
allStationNames:: [Station] -> [SName]
allStationNames = map name

--Demo 2
addStation :: [Station] -> SName -> (North,East) -> [Temperatures] -> [Station]
addStation stations name (north,east) temp = [Station name (north,east) temp] ++ stations   

--DEMO 3

celsToFahrenheit :: [Station] -> [Station]
celsToFahrenheit [] = []
celsToFahrenheit (x:xs) = fromCelsToFahr [x] ++ celsToFahrenheit xs

fromCelsToFahr :: [Station] -> [Station]
fromCelsToFahr [Station name (north,east) temp] = [Station name (north,east) (map ((+32) . (*1.8)) temp)]


--Demo 4

warmerStations :: [Station] -> String -> Float -> [SName]
warmerStations [] _ _ = []
warmerStations (x:xs) months overTemp = monthLocator [x] months overTemp ++ warmerStations xs months overTemp

monthLocator :: [Station] -> String -> Float -> [SName]
monthLocator [Station name (north,east) temp] months overTemp
    |(months == "January" || months == "january") && temp !! 0 > overTemp = name : []
    |(months == "February" || months == "february") && temp !! 1 > overTemp =   name : []
    |(months == "March" || months == "march")&& temp !! 2 > overTemp = name : []
    |(months == "April" || months == "april")&& temp !! 3 > overTemp = name : []
    |(months == "May" || months == "may")&& temp !! 4 > overTemp = name : []
    |(months == "June"|| months == "june") && temp !! 5 > overTemp = name : []
    |(months == "July" || months == "july")&& temp !! 6 > overTemp = name : []
    |(months == "August" || months == "august")&& temp !! 7 > overTemp = name : []
    |(months == "September" || months == "september")&& temp !! 8 > overTemp = name : []
    |(months == "October" || months == "october")&& temp !! 9 > overTemp = name : []
    |(months == "November" || months == "november")&& temp !! 10 > overTemp = name : []
    |(months == "December" || months == "december")&& temp !! 11 > overTemp = name : []
    |otherwise =  []

--Demo 5
stationsToString :: [Station] -> String
stationsToString [] = []
stationsToString (x:xs) = splitStations [x] ++  "\n" ++ stationsToString xs

splitStations :: [Station] -> String
splitStations [Station name (north,east) temp] = "|" ++ name ++ "|" ++ show (decimalSplitter north) ++ "|" ++ show (decimalSplitter east) ++ "|" ++ splitTemps temp     

splitTemps :: [Temperatures] -> String
splitTemps [] = []
splitTemps (x:xs) = "|" ++ show (decimalSplitter x) ++ splitTemps xs

decimalSplitter :: Float -> Float
decimalSplitter number = fromIntegral (round (number*10)) / 10

--Demo 6

stationtempChanger :: [Station] -> String -> String -> Float -> [Station]
stationtempChanger [] _ _  _ = []
stationtempChanger  (x:xs) statName month newTemp = stationLocator [x] statName month newTemp ++ stationtempChanger xs statName month newTemp

stationLocator :: [Station] -> String -> String -> Float -> [Station]
stationLocator [Station name (north,east) temp] statname month newTemp
    | name == statname = monthFinder [Station name (north,east) temp] month newTemp
    | otherwise = Station name (north,east) temp : []

monthFinder :: [Station] -> String -> Float -> [Station]
monthFinder [Station name (north,east) temp] month newTemp 
    | (month == "January" || month == "january")  =  Station name (north,east) (tempChanger  newTemp (temp!!0) temp) : []
    | (month == "February" || month == "february") =  Station name (north,east) (tempChanger  newTemp (temp!!1) temp) : []
    | (month == "March" || month == "march")  =  Station name (north,east) (tempChanger  newTemp (temp!!2) temp) : []
    | (month == "April" || month == "april")  =  Station name (north,east) (tempChanger  newTemp (temp!!3) temp) : []
    | (month == "May" || month == "may")  =  Station name (north,east) (tempChanger  newTemp (temp!!4) temp) : []
    | (month == "June"|| month == "june")  =  Station name (north,east) (tempChanger  newTemp (temp!!5) temp) : []
    | (month == "July" || month == "july")  =  Station name (north,east) (tempChanger  newTemp (temp!!6) temp) : []
    | (month == "August" || month == "august")  =  Station name (north,east) (tempChanger  newTemp (temp!!7) temp) : []
    | (month == "September" || month == "september")  =  Station name (north,east) (tempChanger  newTemp (temp!!8) temp) : []
    | (month == "October" || month == "october")  =  Station name (north,east) (tempChanger  newTemp (temp!!9) temp) : []
    | (month == "November" || month == "november")  =  Station name (north,east) (tempChanger  newTemp (temp!!10) temp) : []
    | (month == "December" || month == "december")  =  Station name (north,east) (tempChanger  newTemp (temp!!11) temp) : []
    | otherwise           =  Station name (north,east) temp : []    

tempChanger :: Float -> Float -> [Temperatures] -> [Temperatures]
tempChanger _ _ [] = []
tempChanger newTemp oldTemp (x:xs) 
   | oldTemp == x = newTemp:[] ++ tempChanger newTemp oldTemp xs
   | otherwise =  x:[] ++ tempChanger newTemp oldTemp xs

--Demo 7




--
--  Demo
--

demo :: Int -> IO ()

demo 1 = print (allStationNames testData)

demo 2 = print (addStation testData "Valley"  (53.252, -4.537)  [8.37, 8.44, 9.84, 12.09, 15.01, 17.24, 18.77, 18.76, 17.26, 14.31, 11.26, 9.09])

demo 3 = print (celsToFahrenheit testData)

demo 4 = print (warmerStations testData "August" 20.00) 
    
demo 5 = putStr (stationsToString testData)

demo 6 = print (stationtempChanger testData "Heathrow" "January" 25.00)

--demo 7 = -- output the name of the nearest weather station to location (50.2N, -0.4E)
--         -- which has a March temperature warmer than 10 degrees Celsius

demo 8 = tempChartAnimation testData months 

--
-- Screen Utilities (use these to do the bar chart)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your bar chart code goes here
--

tempChartAnimation :: [Station] -> [String] -> IO()
tempChartAnimation _ []  = putStr ""
tempChartAnimation [] _ =putStr""
tempChartAnimation stations (x:xs) = do 
    printf (monthsTemp stations (monthSplitter stations x))
    threadDelay 1000000
    clearScreen
    tempChartAnimation stations xs
        

monthsTemp :: [Station] -> Int -> String
monthsTemp [] _ = ""
monthsTemp (x:xs) month = monthChart [x] month ++ "\n" ++ monthsTemp xs month 

monthSplitter :: [Station] -> String -> Int
monthSplitter [] _ = 100
monthSplitter stations month 
    | (month == "January" || month == "january")  = 0
    | (month == "February" || month == "february") = 1
    | (month == "March" || month == "march")  = 2
    | (month == "April" || month == "april")  = 3
    | (month == "May" || month == "may")  = 4
    | (month == "June"|| month == "june")  = 5 
    | (month == "July" || month == "july")  = 6 
    | (month == "August" || month == "august")  = 7 
    | (month == "September" || month == "september")  = 8 
    | (month == "October" || month == "october")  =  9
    | (month == "November" || month == "november")  = 10
    | otherwise           =  11

monthChart ::[Station] -> Int -> String
monthChart [] _ = ""
monthChart [Station name (north,east) temp] month =  name ++ " " ++ concat(replicate (round(temp!!month)) "#")





--
-- Your user interface (and loading/saving) code goes here
--
