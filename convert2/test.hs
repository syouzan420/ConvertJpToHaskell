-- test.hs  Japanese to Haskell test code

teru = (map ((*) (max (succ 3) (pred 6) ) ) [1,2,3,4,5]) 
てる = ((*) 4) 3
ごりら = ((*) (max (succ 3) (pred 6) ) ) 5
abc = ((*) (pred 6) ) (succ 3) 
テスト = (max (succ 3) (pred 6) ) 
すごいやつ = (pred (pred (succ (succ (pred (succ 5) ) ) ) ) ) 

main = do
  (print teru) 
  (print てる) 
  (print ごりら) 
  (print abc) 
  (print テスト) 
  (print すごいやつ) 
