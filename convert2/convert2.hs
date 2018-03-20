import System.IO (IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (isInfixOf, isSuffixOf)

topText :: String
topText = "-- test.hs  Japanese to Haskell test code\n\n"

-- convert test for ghci
ct :: IO()
ct = convert "test.txt" "test.hs"

joinWords :: [String] -> String
joinWords [] = []
joinWords (x:xs) = (x++(joinWords xs))

convertAll :: [(String, String)] -> String -> String
convertAll tps co = unlines $ map (joinWords . (convertLine tps) . makeSentences) (lines co)

-- [("a のそれぞれに b","map b a")...] -> ["
-- ","d","=","[1,3,4]のそれぞれに..."] -> コンバートしたもの 
convertLine :: [(String, String)] -> [String] -> [String]
convertLine tps [] = []
convertLine tps (x:xs)
    | (filter (/=' ') x)==[] = ([x]++(convertLine tps xs))
    | x=="=" = ([" = "]++(convertLine tps xs))
    | x=="は " = ([" = "]++(convertLine tps xs))
    | otherwise = [(convertString tps tps) $ joinWords $ words x]++(convertLine tps xs)

convertString :: [(String, String)] -> [(String, String)] -> String -> String
convertString tps [] [] = []
convertString tps [([],[])] st = st 
convertString tps [] st = st 
convertString tps x [] = []
convertString tps (x:xs) st
    | not (isKeysLast x)
        = if ((head $ getVerbList x) `isInfixOf` st)
             then if ((length (matchKeys st (getVerbList x)))==(length (getObjectList x)))
                     then (makeString (tps) (words $ snd x)
                           (twoListToTupple (getObjectList x) (matchKeys st (getVerbList x))))
                     else st 
             else convertString tps xs st 
    | ((last $ getVerbList x) `isSuffixOf` st)
        = if ((length (matchKeys st (getVerbList x)))==(length (getObjectList x)))
             then (makeString (tps) (words $ snd x)
                      (twoListToTupple (getObjectList x) (matchKeys st (getVerbList x))))
             else st 
    | otherwise = convertString tps xs st 

-- tps -> ["max","a","b"] -> [("a","3"),("b","5")] -> "(max 3 5) 
makeString :: [(String, String)] -> [String] -> [(String, String)] -> String
makeString [] [] [] = []
makeString tps [] [] = []
makeString tps [] t = []
makeString tps (x:xs) t 
  | x>="a" && x<="z" && (length x)==1 = " "++(convertString tps tps (searchFrom t x))++(makeString tps xs t)
  | x=="main" || x=="do" = x
  | x=="if" || x=="then" || x=="else" = x++(makeString tps xs t)
  | x=="(" || x==")" || x=="." = x++(makeString tps xs t) 
  | otherwise = "("++x++(makeString tps xs t)++")"

searchFrom :: [(String, String)] -> String -> String
searchFrom [] s = []
searchFrom (x:xs) s 
  | (fst x)==s = (snd x)
  | otherwise = searchFrom xs s

twoListToTupple :: [String] -> [String] -> [(String, String)]
twoListToTupple [] [] = []
twoListToTupple (x:xs) (y:ys) = ([(x,y)]++(twoListToTupple xs ys))

getVerbList :: (String, String) -> [String]
getVerbList tp = fst $ getKeys $ fst tp

getObjectList :: (String, String) -> [String]
getObjectList tp = snd $ getKeys $ fst tp

-- "あかとあおがすきです" ["と","がすきです"] -> ["あか","あお"]
matchKeys :: String -> [String] -> [String]
matchKeys [] [] = [] 
matchKeys st [] = [st]
matchKeys st (x:xs)
    | x==[] = [st]
    | st==[] = [] 
    | ((length st) - (length x))==0 = []
    | x `isSuffixOf` st = [take ((length st) - (length x)) st]
    | (head $ splitOn x st)==st = [st]
    | (head $ splitOn x st)=="" =tail (splitOn x st)
    | (last $ splitOn x st)=="" =splitOn x st
    | otherwise = (init $ splitOn x st)++(matchKeys (last $ splitOn x st) xs )

-- "a のそれぞれに b" -> (["それぞれに"],["a","b"])
-- "map b a" -> (["map"],["b","a"])
getKeys :: String -> ([String], [String])
getKeys [] = ([],[])
getKeys st = ((filter (\x -> not (x>="a" && x<="z" && (length x)==1)) (words st)),
             (filter (\x -> (x>="a" && x<="z" && (length x)==1)) (words st)))


isKeysLast :: (String, String) -> Bool
isKeysLast ([],[]) = False
isKeysLast tp = not (x>="a" && x<="z" && (length x)==1)
                    where x = last $ words $ fst tp


makeHText :: FilePath -> FilePath -> [(String, String)] -> IO ()
makeHText fname1 fname2 tps = do
    hin <- openFile fname1 ReadMode
    hout <- openFile fname2 WriteMode
    hSetEncoding hin utf8
    co <- hGetContents hin
    hSetEncoding hout utf8
    let result = (topText++(convertAll tps co))
    hPutStr hout result 
    putStrLn "-----Original Text-----"
    mapM_ putStrLn (lines co)
    putStrLn "-----Converted Text------"
    putStrLn result 
    hClose hin
    hClose hout

makeTupple :: String -> (String, String)
makeTupple [] = ([],[])
makeTupple ln = (head (splitOn " = " ln ) , last (splitOn " = " ln))

addStringToList :: String -> String -> [String]
addStringToList s (x:xs)
      | x==' ' = addStringToList (s++" ") xs
      | s=="" && ("は " `isInfixOf` (x:xs)) = stringInto "は " (x:xs)
      | s=="" && ("=" `isInfixOf` (x:xs)) = stringInto "=" (x:xs)
      | s=="" = [(x:xs)]
      | otherwise = s:(addStringToList "" (x:xs))

makeSentences :: String -> [String]
makeSentences w
      |w=="" = []
      |otherwise = addStringToList "" w 

stringInto :: String -> String -> [String]
stringInto x y = (listInto x) $ splitOn x y

listInto :: String -> [String] -> [String]
listInto x [] = []
listInto x (y:ys)
    | ys /= [] = [y]++[x]++(listInto x ys)
    | otherwise = [y]

convert :: FilePath -> FilePath -> IO () 
convert fname1 fname2 = do
    h <- openFile "converter2.txt" ReadMode
    hSetEncoding h utf8
    c <- hGetContents h
    let tps = map makeTupple $ lines c 
    print tps
    makeHText fname1 fname2 tps
    hClose h

main :: IO ()
main = do
    [fname1, fname2] <- getArgs
    convert fname1 fname2


