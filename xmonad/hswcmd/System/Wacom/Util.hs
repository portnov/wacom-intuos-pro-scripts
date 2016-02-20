
module System.Wacom.Util where

import Data.Char
import Data.List
import qualified Data.ByteString as B

-- | Trim spaces from right and left
trim :: String -> String
trim = \xs -> rtrim $ ltrim xs
  where
    ltrim = dropWhile isSpace
    rtrim [] = []
    rtrim xs = if isSpace $ last xs
                 then rtrim $ init xs
                 else xs

trimQuotes :: String -> String
trimQuotes [] = []
trimQuotes ('"' : xs) = rtrim xs
  where
    rtrim [] = []
    rtrim xs = case last xs of
                 '"' -> init xs
                 _ -> xs

dropLastWord :: String -> String
dropLastWord = init . dropWhileEnd (not . isSpace)

fromBS :: B.ByteString -> String
fromBS = map (chr . fromIntegral) . B.unpack

isWacom :: String -> Bool
isWacom dev = "Wacom" `isInfixOf` dev

