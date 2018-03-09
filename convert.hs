import System.Environment (getArgs)
import System.IO

-- ConvertList(cl) はタプルを要素とするリストである
-- x といふ文字列が タプルの第一要素に合致すれば
-- そのタプルを要素としたリストを返す
-- このリストは要素数0もしくは1となる筈である
fitlist :: [(String,String)] -> String -> [(String, String)]
fitlist cl x = [y | y <- cl, (fst y)==x]

-- 第一引數は fitlistへの引きわたしのためにある
-- 文字列を與へたとき それが ConvertListにあれば タプルの第二要素が返る
-- なければ fitlist の結果は空リストなので その時は 文字列をそのまま返す
convert :: [(String, String)] -> String -> String
convert cl x =
    if (fitlist cl x)==[]
      then x
      else snd $ head (fitlist cl x)

-- 第二引數は cl をconvert へ引き繼がせてゐる
-- conといふ第一引數は ファイル中の全文字列である
-- これを行に分け さらに單語に分けて その單語をconvertで變換し
-- 單語をつないで文にし それを複数行つなげて 文章全體に戻す
convertAll :: String -> [(String, String)] -> String
convertAll con cl= unlines $ map unwords $ map (map (convert cl)) (map words $ lines con)

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

-- 書き込むファイルの先頭に加へる文章
-- import文を加へて 新たに定義した函數が使へるやうにする
topText :: String
topText = "import newFunc\n\n"

-- コマンド引數をふたつ(ファイル名)とり
-- readConverter を實行するのみ これがメイン函數となる
main :: IO ()
main = do
    [fname1, fname2] <- getArgs
    readConverter "converter.txt" fname1 fname2

--以前に書いたものを ここにまとめる
--
--convert :: String -> String
--convert x 
--    | x=="とる" = "take"
--    | x=="みなやる" = "map"
--    | x=="みなたす" = "sum"
--    | x=="`あまり`" = "`mod`"
--    | otherwise = x
--
--convertAll :: String -> String
--convertAll x = unlines (map unwords (map (map convert) (map words (lines x))))
--
--convertList = [("とる","take"),("みなやる","map"),("みなたす","sum"),
--              ("つぎ","succ"),("おう","max"),("すす","min"),("わる","div")]
