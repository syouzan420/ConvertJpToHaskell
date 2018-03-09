import System.Environment (getArgs)
import Data.List (isInfixOf)
import System.IO (IOMode(..), openFile, hClose, hGetContents, hSetEncoding, utf8, hPutStr)

-- ConvertList(cl) はタプルを要素とするリストである
-- x といふ文字列が タプルの第一要素に含まれてゐれば
-- そのタプルを要素としたリストを返す
fitlist :: [(String,String)] -> String -> [(String, String)]
fitlist cl x = [y | y <- cl, (fst y) `isInfixOf` x]

-- これはfitlistとほぼ同じだが
-- x がタプルの第一要素に完全一致する場合のものだ
justFitlist :: [(String,String)] -> String -> [(String, String)]
justFitlist cl x = [y | y <- cl, (fst y)==x]

-- 第一引數は fitlistへの引きわたしのためにある
-- まづ x が 用意した文字列の一部にも該当しない場合 x をそのまま返す
-- x が 用意した文字列のどれかに 完全一致するとき
-- 完全一致したタプルの第二要素(變換した文字列)を返す
-- さうでなければ x が用意した文字列に一致した部分を變換し
-- その前についてゐる文字列(これは 函數の引數にあたる)を 變換後のxの後ろへ
-- もってくる changePositionなる函數を適用する
-- 例へば 用意したタプルが("たす","+")であった場合
-- 4たす といふ文字列は (+ 4)と變換される！
convert :: [(String, String)] -> String -> String
convert cl x 
    | ((fitlist cl x)==[]) && ((last x)=='は') = (init x)++" = "
    | (fitlist cl x)==[] = x
    | (justFitlist cl x)/=[] = (snd $ head (justFitlist cl x))
    | (last x)=='は' = (init (snd $ head (fitlist cl x)))++" = "
    | otherwise = changePosition cl (fst $ head (fitlist cl x)) x []

-- これは 與へられた文字列(x:xs) に對して wといふ共通部分があれば
-- それを cl で示された變換法則に從つて變換し 共通部分の前にあった文字列を
-- 變換した文字列の後に 空白を挿入しながら配置し その全部を括弧でくくる
-- といふことをする函數である
changePosition :: [(String,String)] -> String -> String -> String ->String
changePosition cl w (x:xs) li
    | xs==[] = (x:xs)
    | xs==w && x=='を' = (snd $ head (fitlist cl w))++[' ']++li
    | xs==w = ['(']++(snd $ head (fitlist cl w))++[' ']++li++[x]++[')']
    | otherwise = changePosition cl w xs (li++[x])

-- 第二引數は cl をconvert へ引き繼がせてゐる
-- conといふ第一引數は ファイル中の全文字列である
-- これを行に分け さらに單語に分けて その單語をconvertで變換し
-- 單語をつないで文にし それを複数行つなげて 文章全體に戻す
convertAll :: String -> [(String, String)] -> String
convertAll con cl= unlines $ map unwords $ map (map (convert cl)) (map words' $ lines con)

-- fname1 はソース元のファイル名である
-- fname2 は變換ごのファイル名であり 末尾は.hsとしたい
-- 引數clは convertAll へ引きついでゐるもので ConvertListのことだ
-- ソースファイルを開いて内容をcoといふ變數に読みこみ
-- convertALlでcoを變換したものの先頭に topText を加へてfname2の方へ書きこんでゐる
convertText :: FilePath -> FilePath -> [(String, String)] -> IO ()
convertText fname1 fname2 cl = do
    hin <- openFile fname1 ReadMode
    hout <- openFile fname2 WriteMode
    hSetEncoding hin utf8
    co <- hGetContents hin
    let con = topText ++ co
    hSetEncoding hout utf8
    hPutStr hout (convertAll con cl) 
    print (map words' $ lines co)
    hClose hin
    hClose hout

-- fnameは "converter.txt"のことで 文字の變換則を記述したファイルだ
-- fname1 fname2については上記に示した
-- "converter.txt"の内容を読みこみ 行 さらに 單語に分けて
-- 變換前 變換後の文字列をタプルにし そのリストをつくる
-- これが ConvertList(cl)である この變數が
-- 別の関数へどんどん渡されてゐき fitlist でその効果を発揮する
readConverter :: FilePath -> FilePath -> FilePath -> IO ()
readConverter fname fname1 fname2 = do
    h <- openFile fname ReadMode
    hSetEncoding h utf8
    cn <- hGetContents h
    let cl = map makeTupple $ map words $ lines cn
    convertText fname1 fname2 cl
    hClose h

-- ふたつの要素よりなる文字列リストをタプルに變換する函數
-- ふたつの要素でなくとも動くが 今回變換したいものは 要素がふたつに限る
makeTupple :: [String] -> (String, String)
makeTupple l = (head l, head $ tail l)

-- 以下の二つの函數は words 函數が
-- 行の冒頭にあるスペースをすべて省いてしまふ
-- といふ問題を解決するためつくられた
tabAdd :: String -> String -> [String]
tabAdd s (x:xs)
      | x==' ' = tabAdd (s++" ") xs
      | s=="" = words (x:xs)
      | otherwise = s:(words (x:xs)) 

words' :: String -> [String]
words' w
      |w=="" = []
      |otherwise = tabAdd "" w 

-- 書き込むファイルの先頭に加へる文章
-- import文を加へて 新たに定義した函數が使へるやうにする
topText :: String
topText = "import NewFunc\n\n"

-- コマンド引數をふたつ(ファイル名)とり
-- readConverter を實行するのみ これがメイン函數となる
main :: IO ()
main = do
    [fname1, fname2] <- getArgs
    readConverter "converter.txt" fname1 fname2

