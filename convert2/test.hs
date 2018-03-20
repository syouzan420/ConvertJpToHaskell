-- test.hs  Japanese to Haskell test code

teru = (map (* (max (succ 3) (pred 6))) [1,2,3,4,5])
てる = ( (* 4) 3)
くり = ( (+ 6) 5)
ごりら = ( (* (max (succ 3) (pred 6))) 5)
abc = ( (* (pred 6)) (succ 3))
テスト = (max (succ 3) (pred 6))
すごいやつ = (pred (pred (succ (succ (pred (succ 5))))))
a = ( (`div` 2) 5)
b =  (+ 10) ( (* 5) 3)
c = 3*5+10
d = (map ( (+ 5). (* 4)) [3..10])
ex = if x>10
      then 100
      else 200

main = do
  (print teru)
  (print てる)
  (print ごりら)
  (print abc)
  (print テスト)
  (print すごいやつ)
  (print a)
  (print b)
