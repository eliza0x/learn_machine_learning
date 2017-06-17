module Logic
    ( main
    , andUnit
    , orUnit
    , nandUnit
    , norUnit
    , xorUnit
    , step
    ) where

main :: IO ()
main = do
    putStr "input: "
    [a, b] <- map toInt . words <$> getLine :: IO [Int]
    putStrLn $ "OR: "   ++ show (orUnit   a b)
    putStrLn $ "AND: "  ++ show (andUnit  a b)
    putStrLn $ "NAND: " ++ show (nandUnit a b)
    putStrLn $ "NOR: "  ++ show (norUnit  a b)
    putStrLn $ "XOR: "  ++ show (xorUnit  a b)
    where
      toInt = read :: String -> Int

andUnit, orUnit, nandUnit, norUnit, xorUnit :: Int -> Int -> Int
andUnit  n m = step $ unit 1    1    (-1) n m
orUnit   n m = step $ unit 1    1    0    n m
nandUnit n m = step $ unit (-1) (-1) 2    n m
norUnit  n m = step $ unit (-1) (-1) 1    n m
xorUnit  n m = step $ andUnit (orUnit n m) (nandUnit n m) -- | 2 layer

unit :: Int -- first weight
     -> Int -- second weight
     -> Int -- bias
     -> Int -- first input
     -> Int -- second input
     -> Int -- output
unit w w' b n m = n * w + m * w' + b

step :: Int -> Int
step n = if n >= 1 then 1 else 0

