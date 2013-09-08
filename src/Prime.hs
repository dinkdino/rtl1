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
        
mnm' :: (Ord a) => [a] -> a
mnm' [] = error "Empty list"
mnm' [x]  = x
mnm' (x:xs) = min' x (mnm' xs)


searchFst :: (Eq a) => a -> [a] -> Int -> Int
searchFst _ [] _ = -1
searchFst n (x:xs) c
                    | n == x     = c 
                    | otherwise  = searchFst n xs (c+1)

removeFst :: (Eq a) => a -> [a] -> [a]
removeFst _ [] = []
removeFst m (x:xs)
                  | p < 0     = x:xs
                  | otherwise = let (ys, zs) = splitAt p (x:xs) in ys ++ (tail zs)
                  where p = searchFst m (x:xs) 0

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = let m = mnm' xs in m : srtInts (removeFst m xs)

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

length' :: [a] => Int
length' [] = 0
length' xs = 1 + length' xs

avg' :: [Int] -> Float
avg' [] = 0
avg' xs = fromIntegral (sum xs) / fromIntegral (length xs)

countIter :: (Eq a) => a -> [a] -> Int -> Int
countIter _ [] _ = 0
countIter n (x:xs) c
                    | n == x = 1 + countIter n xs (c+1)
                    | otherwise = countIter n xs c

count' :: (Eq a) => a -> [a] -> Int
count' _ [] = 0
count' n (x:xs) = countIter n (x:xs) 0

count'' :: (Eq a) => a -> [a] -> Int
count'' _ [] = 0
count'' n (x:xs)
                | ys == (x:xs) = 0
                | otherwise = 1 + count'' n ys
                where ys = removeFst n (x:xs)
                
repeat' :: Int -> a -> [a]
repeat' c x
           | c < 1 = error "count cannot be negative" 
           | c == 1 = [x]
           | otherwise = x : repeat' (c-1) x

blowUpIter' :: [a] -> Int -> [a]
blowUpIter' [] _ = []
blowUpIter' (x:xs) c = repeat' c x ++ blowUpIter' xs (c+1)

blowUp' :: [a] -> [a]
blowUp' xs = blowUpIter' xs 1 

srtString' [] = []
srtString' (x:xs) = m : srtString' (removeFst m (x:xs))
                 where m = mnm' (x:xs)

prefixIter [] _ cs = cs
prefixIter _ [] cs = cs
prefixIter (y:ys) (z:zs) cs 
                           | y == z = prefixIter ys zs (cs ++ [y])
                           | otherwise = cs

prefix ys zs
            | p == ys   = True
            | otherwise = False
            where p = prefixIter ys zs []
                    
prefix' [] ys = True
prefix' xs [] = False
prefix' (x:xs) (y:ys) = (x == y) && prefix' xs ys
          
 
substr' [] _ = True
substr' _ [] = False
substr' (x:xs) (y:ys)
             | (x:xs) `prefix'` (y:ys) = True
             | otherwise = substr' (x:xs) ys








