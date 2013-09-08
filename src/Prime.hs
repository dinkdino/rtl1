module Prime where

divides :: (Integral a) => a -> a -> Bool
n `divides` d = d `rem` n == 0

ld :: (Integral a) => a -> a
ld n = ldf 2 n

ldf :: (Integral a) => a -> a -> a
ldf k n 
       | k `divides` n = k
       | k^2 > n       = n
       | otherwise     = ldf (k+1) n

prime' :: (Integral a) => a-> Bool
prime' n
        | n < 1     = error "Not a positive number"
        | n == 1    = False
        | otherwise = ld n == n
        
min' :: (Ord a) => a -> a -> a
min' x y
        | x < y     = x
        | otherwise = y
        
mnmInts :: [Int] -> Int
mnmInts [] = error "Empty list"
mnmInts [x]  = x
mnmInts (x:xs) = min' x (mnmInts xs)


searchFst _ [] _ = -1
searchFst n (x:xs) c
                    | n == x     = c 
                    | otherwise  = searchFst n xs (c+1)

removeFst _ [] = []
removeFst m (x:xs)
                  | p < 0     = x:xs
                  | otherwise = let (ys, zs) = splitAt p (x:xs) in ys ++ (tail zs)
                  where p = searchFst m (x:xs) 0








