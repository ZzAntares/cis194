type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ =
  [(a, b)]

hanoi disks a b c =
  (hanoi (disks - 1) a c b) ++ [(a, b)] ++ (hanoi (disks - 1) c b a)
