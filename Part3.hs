-- Name: Part3a.hs
-- Description: Slides _ - _ in the Amuse-Bouche lecture at https://www.youtube.com/watch?v=b9FagOVqxmI&feature=youtu.be
-- Author: Austin Vargason
import Data.Maybe
import Data.Map
import Data.Text
import Network.HTTP
import Data.String
import Data.Time.Calendar
import Control.Applicative
import Data.List

-- definining Maybe
-- Maybe is part of the standard library so we have it commented out here
-- A Maybe can act as a wrapper for a type to represent the data of that type or Nothing if the value does not meet the characteristic
-- This helps a lot with the issue of dealing with Nulls or doing things like else return value == -1
-- data Maybe a = Nothing | Just a

-- Examples of using a Maybe

-- elemIndex function
-- this function is able to return the index of an element provided in the list provided if it is able to fit the type Int, otherwise we return a nothing
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex e (x:xs) = 
    case [ i | (i, b) <- Prelude.zip [0..] (x:xs), b == e] of
        i:_ -> Just i
        [] -> Nothing

-- lookup function
-- this function is able to return the value found for a key in the list
myLookup :: Eq k => k -> Map k a -> Maybe a
myLookup k map = 
    case [ z | (b, z) <- (toList map), b == k ]  of 
        v:_ -> Just v
        [] -> Nothing

-- stripPrefix function
-- this function strips a prefix from a Text and returns a nothing if the prefix is not found in the string
myStripPrefix :: Text -> Text -> Maybe Text
myStripPrefix pre total =
    let prefix = Data.Text.take (Data.Text.length pre) total
    in if prefix == pre
        then Just $ replace pre (fromString "") total
        else Nothing

-- port function
-- this function will extract the port member from the URIAuthority data type as a Maybe Int, needs Network.HTTP.Base
--port :: URIAuthority -> Maybe Int

-- to convert from a char array to a string
toString :: [Char] -> String
toString val = val 

-- our test to test getting the port from the URIAuthority
testPort :: Maybe Int
testPort = 
    let test = URIAuthority (Just $ toString "austin") (Just $ toString "test") (toString "www.google.com") (Just 22)
    in port test


---- Part 3a from github for Amuse Bouche Lecture

firstOne :: [a] -> a -- normally called 'head'
firstOne (a:_) = a
firstOne [] = error "O Noes!"

firstOne' :: [a] -> Maybe a
firstOne' (a:_) = Just a
firstOne' [] = Nothing


addAWeek :: Day -> Day
addAWeek d = addDays 7 d

interestingDates :: [Day]
interestingDates =
    [ fromGregorian 1966  9  8 -- first episode of Star Trek airs
    , fromGregorian 1969  6 21 -- first person on the moon
    , fromGregorian 1969 10 29 -- first ARPANET message sent
    ]
    
anInterestingDate :: Maybe Day
anInterestingDate = firstOne' interestingDates

aWeekLater :: Maybe Day
aWeekLater = fmap addAWeek anInterestingDate

maybeAddAWeek :: Maybe Day -> Maybe Day
maybeAddAWeek = fmap addAWeek

aWeekLater' :: Maybe Day
aWeekLater' = maybeAddAWeek anInterestingDate

-- Part 3b from github

tvShows :: [(Int, String)] -- a list of pairs
tvShows = 
    [ (1966, "Star Trek")
    , (1969, "Monty Python's Flying Circus")
    , (1989, "The Simpsons")
    ]

showForYear :: Int -> Maybe String
showForYear y = lookup y tvShows
    -- lookup "lookup" w/Hoogle: http://www.haskell.org/hoogle/?hoogle=lookup

showWithName :: String -> Maybe String
showWithName n = (listToMaybe . filter (isInfixOf n) . map snd) tvShows
    -- for a good exercise, figure out what this does
    -- look these functions up in Hoogle (just follow the first hit for each)

favoriteShow :: String -> Maybe String
favoriteShow "Amy" = Just "Batman"
favoriteShow "Bob" = Just "Iron Chef"
favoriteShow _     = Nothing


data Person = Person { name :: String, year :: Int }
    -- This has "named" fields, which act as accessor functions

amy = Person { name = "Amy", year = 1971 }
cam = Person { name = "Cam", year = 1989 }
deb = Person { name = "Deb", year = 1967 }
monty = Person { name = "Monty", year = 1973 }

pickShow :: Person -> Maybe String
pickShow p =
    favoriteShow (name p)
    <|> showWithName (name p)
    <|> showForYear (year p)



        
