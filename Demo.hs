data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

data FailableDouble = Failure
                    | OK Double
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv a b = OK (a / b)

failureToZero :: FailableDouble -> Double
failureToZero Failure    = 0
failureToZero (OK value) = value

-- Store a person's name, age and favourite Thing.
data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ age _) = age


data OtherPerson = Worker String Int Thing
                 | Researcher String Int Thing
  deriving Show

joe :: OtherPerson
joe = Worker "Mechanic" 12 Ship

mike :: OtherPerson
mike = Researcher "The string theory" 3 Cabbage

getMembershipAge :: OtherPerson -> Int
getMembershipAge (Worker _ age _)       = age
getMembershipAge (Researcher _ since _) = since

-- Named pattern matching example
baz :: Person -> String
baz person@(Person name _ _) = "The type (" ++ (show person) ++ ") has the name: " ++ name

-- Nested pattern matching
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", you're favorite thing is lame!"

-- Case expression
ex03 = case "Hello" of
  []      -> 3
  ('H':s) -> length s
  _       -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x =
  case x of
    Failure    -> 0
    (OK value) -> value

-- Recursive data type
data IntList = Empty
             | Cons Int IntList
  deriving Show

ex04 :: IntList
ex04 = Cons 3 $ Cons 4 Empty

intListProd :: IntList -> Int
intListProd Empty        = 1
intListProd (Cons x lst) = x * intListProd lst

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
