import System.IO (IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (isInfixOf, isSuffixOf)

topText :: String
topText = "import NewFunc\n\n"

-- convert test for ghci
ct :: IO()
ct = convert "test.txt" "test.hs"

joinWords :: [String] -> String
joinWords [] = []
joinWords (x:xs) = (x++(joinWords xs))

convertAll :: [(String, String)] -> String -> String
convertAll tps co = unlines $ map (convertString tps tps) $ map (joinWords . words) (lines co)

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
                     else convertString tps xs st 
             else convertString tps xs st 
    | ((last $ getVerbList x) `isSuffixOf` st)
        = if ((length (matchKeys st (getVerbList x)))==(length (getObjectList x)))
             then (makeString (tps) (words $ snd x)
                      (twoListToTupple (getObjectList x) (matchKeys st (getVerbList x))))
             else convertString tps xs st 
    | otherwise = convertString tps xs st 

makeString :: [(String, String)] -> [String] -> [(String, String)] -> String
makeString [] [] [] = []
makeString tps [] [] = []
makeString tps [] t = []
makeString tps (x:xs) t 
  | x>="a" && x<="z" && (length x)==1 = (convertString tps tps (searchFrom t x))++(makeString tps xs t)
  | otherwise = "("++x++" "++(makeString tps xs t)++") "

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
    | x `isSuffixOf` st = [take ((length st) - (length x)) st]
    | (head $ splitOn x st)==st = [st]
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


