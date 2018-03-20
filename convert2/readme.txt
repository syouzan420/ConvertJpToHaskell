(2018/3/20)問題點
函數名の後 引数を記述するため スペースを入れると 變換後 スペースが失なはれてしまふといふ致命的バグ

->(2018/3/20)
行において "=" の左側は 變換則を使はないやうに修正し解決

convertLine tps (x:xs)を
convertLine tps li@(x:xs)とし
  | (["="] `isInfixOf`li) || (["は "] `isInfixOf` li) = ([x]++(convertLine tps xs))

と修正
