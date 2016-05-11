import Language.Haskell.Ghcid
import System.Console.ANSI
import Data.String.Utils

main = do
  (ghci, _) <- startGhci "ghci" (Just ".") True
  iter ghci

iter ghci = do
  let executeStatement = exec ghci
  putStr "ghci>> "
  input <- getLine
  result <- executeStatement $ input
  let cRes = parse . unlines $ result
  putStr cRes
  iter ghci

parse :: String -> String
parse "" = ""
parse str
  | s `elem` digits = (colorizeDigit s)++(parse ss)
  | s `elem` brackets = (colorizeBracket s)++(parse ss)
  | any (==True) [arrow `startswith` str | arrow <- arrows] = (colorizeArrow . (take 2) $ str) ++ (parse . tail $ ss)
  | any (==True) [tClass `startswith` str | tClass <- typeclasses] = (colorizeTypeclass sStr) ++ (parse nStr)
  | any (==True) [sWord `startswith` str | sWord <- successWords] = (colorizeSuccessWord sStr) ++ (parse nStr)
  | s == '\"' = (colorizeSubstring subStr) ++ (parse nextStr)
  | otherwise = s:(parse ss)
  where
    (s:ss) = str
    (subStr, nextStr) = extractString ss
    (sStr, nStr) = extractSubstring str

green  = "\x1b[32b"
red    = "\x1b[31m"
blue   = "\x1b[94m"
yellow = "\x1b[33m"
def    = "\x1b[0m"
purple = "\x1b[95m"


arrows :: [String]
typeclasses :: [String]
brackets :: [Char]
digits :: [Char]

arrows = ["->", "=>"]	
brackets = ['(', ')', '[', ']', '{', '}']
digits = ['0'..'9']
typeclasses = ["Eq", "Ord", "Read", "Show", "Num"]
successWords = ["Ok", "done"]

colorizeChar :: String -> Char -> String
colorizeString :: String -> String -> String

colorizeChar color ch = color ++ [ch] ++ def
colorizeString color str = color ++ str ++ def

colorizeDigit = colorizeChar purple 
colorizeBracket = colorizeChar blue

colorizeTypeclass :: String -> String
colorizeTypeclass = colorizeString blue

colorizeArrow :: String -> String
colorizeArrow = colorizeString red

colorizeSubstring :: String -> String
colorizeSubstring = colorizeString yellow

colorizeSuccessWord :: String -> String
colorizeSuccessWord = colorizeString green

extractString :: String -> (String, String)
extractString str = (str1, str2)
  where
  str1 = ('\"':takeWhile (/= '\"') str) ++ "\""
  str2 = tail $ (dropWhile (/= '\"') str)

extractSubstring :: String -> (String, String)
extractSubstring str = (str1, str2)
  where
  str1 = takeWhile (/= ' ') str
  str2 = dropWhile (/= ' ') $ str
