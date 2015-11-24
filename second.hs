{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Data.ByteString.Internal (unpackBytes)
import Data.ByteString.Char8 (pack, unpack)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Char
import qualified Data.AttoBencode as BEncode
import qualified Data.ByteString.Lazy as B
import GHC.Word (Word8)
import Text.Printf
import Data.List 
import Text.Parsec

--------------(   V  ,    X  ,    Y  )
type Action = (String, String, String)
type Cords = (String, String)
type Board = [Action]

format = "application/bencode+list"

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  putStrLn "Please enter unique game name:"
  gameName <- getLine
  let gameUrl = "http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/1"

      gameUrl2 = "http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/2"

--------ne mano 1
--  postMove manager gameUrl $ encode $ [("x", "1", "1")] 

---------mano 2
  moves <- getMove manager gameUrl2
  postMove manager gameUrl2 $ encode $ moves ++ [("o", "0", "2")]

-----------ne mano 3
--  moves <- getMove manager gameUrl
--  postMove manager gameUrl $ encode $ moves ++ [("x", "2", "2")]

--------------mano 4
  moves <- getMove manager gameUrl2
  postMove manager gameUrl2 $ encode $ moves ++ [("o", "0", "0")]

-------ne mano  5
--  moves <- getMove manager gameUrl
--  postMove manager gameUrl $ encode $ moves ++ [("x", "0", "1")]

----------- mano 6
  moves <- getMove manager gameUrl2
  postMove manager gameUrl2 $ encode $ moves ++ [("o", "2", "1")]

-------ne mano  7
--  moves <- getMove manager gameUrl
--  postMove manager gameUrl $ encode $ moves ++ [("x", "1", "2")]

----------mano 8
  moves <- getMove manager gameUrl2
  postMove manager gameUrl2 $ encode $ moves ++ [("o", "1", "0")]

  -------ne mano  9
--  moves <- getMove manager gameUrl
--  postMove manager gameUrl $ encode $ moves ++ [("x", "2", "0")]

  print $ "Game finished"
  
getMove :: Manager -> String -> IO Board
getMove manager url = do
  request <- parseUrl url
  let request' = request { method = "GET"
                         , requestHeaders = [("Accept", format)]}
  response <- httpLbs request' manager
  let moves = decode $ unpack (B.toStrict (responseBody response))
  Prelude.putStr $ "Moves received: "
  print moves
  return $ moves

postMove :: Manager -> String -> String -> IO ()
postMove manager url move = do
  initialRequest <- parseUrl url
  let request = initialRequest { method = "POST"
                               , requestBody = RequestBodyLBS $ B.pack $ unpackBytes.pack $ move
                               , requestHeaders = [("Content-Type", format)]}
  response <- httpLbs request manager
  Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

encode :: Board -> String
encode encode = addToList str
    where str = intercalate "" sqStrings
          sqStrings = map encodeToString encode

encodeToString :: (String, String, String) -> String
encodeToString map = 
  let 
      stringV = unpack $ BEncode.encode $ pack "v"
      stringV2 = unpack (BEncode.encode (pack (value map "v")))
      stringX = unpack $ BEncode.encode $ pack "x"
      stringX2 = unpack $ BEncode.encode $ digitToInt $ value map "x" !! 0
      stringY = unpack $ BEncode.encode $ pack "y"
      stringY2 = unpack $ BEncode.encode $ digitToInt $ value map "y" !! 0

      stringAll = "d" ++ stringV ++ stringV2 ++ stringX ++ stringX2 ++ stringY ++ stringY2 ++ "e"
  in (stringAll)

addToList :: String -> String
addToList list = printf format list
    where format = "l%se"

------------------------Pirma Progama

value :: Action -> String -> String
value (v0,_,_) "v" = v0
value (_,v1,_) "x" = v1
value (_,_,v2) "y" = v2

decode :: String -> Board
decode ('l' : msg) = 
    let
    (external, _) = decodeBoard msg []
    in external
decode _ = error "No message"

decodeBoard :: String -> Board -> (Board, String)
decodeBoard ('e':left) acc = (acc, left)
decodeBoard ('d':dict) acc =
    let
    (d,left) = readAction dict
    in decodeBoard left (d:acc)
decodeBoard str ext = error ("Invalid message. Unparsed content: " ++ str) 

readAction :: String -> (Action, String)
readAction str =
    let
        (key1, val1, left) = readItem str
        (key2, val2, left2) = readItem left
        (key3, val3, left3) = readItem left2
        ('e' : left4) = left3
        items = [(key1, val1),(key2, val2),(key3, val3)]
        v1 = match (=='v') items
        v2 = match (=='x') items
        v3 = match (=='y') items
    in ((v1,v2,v3), left4)

readCords :: String -> (Cords, String)
readCords str =
    let
        (key1, val1, left) = readItem str
        (key2, val2, left2) = readItem left
        (key3, val3, left3) = readItem left2
        ('e' : left4) = left3
        items = [(key1, val1),(key2, val2),(key3, val3)]
        v1 = match (=='v') items
        v2 = match (=='x') items
        v3 = match (=='y') items
    in ((v2,v3), left4)

readItem :: String -> (String, String, String)
readItem str =
    let
        (key, left) = readSymbol str
        (value, left') = readSymbol left
    in (key, value, left')

readSymbol :: String -> (String, String)
readSymbol ('1' : ':' : rest) = readLetter rest
readSymbol ('i' : rest) = readNumber rest
readSymbol str = error ("Invalid message. Unparsed content: " ++ str)

readLetter :: String -> (String, String)
readLetter ( a : rest) = ([a], rest)

readNumber :: String -> (String, String)
readNumber str = 
    let
        num = takeWhile (/='e') str
        left = drop (length num + 1) str
    in (num, left)

match :: (Char -> Bool) -> [(String, String)] -> String
match mch [] = ""
match mch (([],v) : rest) = match mch rest
match mch ( ((b:_),v) : rest) = 
    if 
        mch b
    then 
        v 
    else (match mch rest)