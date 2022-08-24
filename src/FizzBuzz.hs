import Data.Foldable (foldl')

fizzBuzz = let
    makeCycle word num = cycle([word] ++ (take (num - 1) (repeat "")))
    fizzes = makeCycle "Fizz" 3
    buzzes = makeCycle "Buzz" 5
    strings = zipWith (++) fizzes buzzes
    in zipWith (\ str num -> if (null str) then show num else str) strings [0..]

main = foldl' (*>) (pure ()) (map putStrLn (drop 1 $ take 1000001 fizzBuzz))
