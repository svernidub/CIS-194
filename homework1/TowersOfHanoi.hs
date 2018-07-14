module TowersOfHanoi (
    hanoi
) where

type Peg = String
type Move = (Peg, Peg)


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _      _    _    = []
hanoi n s d t = step1 ++ step2 ++ step3 
                    where
                        step1 = hanoi (n - 1) source temp dest
                        step2 = [(source, dest)]
                        step3 = hanoi (n - 1) temp dest source
