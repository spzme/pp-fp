import Data.Char
import Data.List
import Test.QuickCheck

myfilter:: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) | f x = [x] ++ myfilter f xs
                  | otherwise = myfilter f xs

myfilter':: (a -> Bool) -> [a] -> [a]
myfilter' f xs = [x | x <- xs, f x]

myfoldl:: (a -> b -> a) -> a -> [b] -> a
myfoldl f z [] = z
myfoldl f z (x:xs) = myfoldl f z xs `f` x

myfoldr:: (a -> b -> b) -> b -> [a] -> b
myfoldr f z [] = z
myfoldr f z (x:xs) = x `f` myfoldr f z xs

myZipWith:: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f _ [] = []
myZipWith f [] _ = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys

type Person = (String, Int, String, String)

database:: [Person]
database = [("Barry Pooter", 21, "Male", "Zweinstein"),
            ("Hermelientje", 19, "Female", "Hagrid's Hut"),
            ("Herta", 33, "Female", "Utopia")]

name (x,_,_,_) = x
age (_,x,_,_) = x
sex (_,_,x,_) = x
por (_,_,_,x) = x     

increaseAge n [] = []
increaseAge n ((a,b,c,d):xs) = (a,b+n,c,d) : (increaseAge n xs)

increaseAge' n xs = [(a,b+n,c,d) | (a,b,c,d) <- xs]

hoInc n (a,b,c,d) = (a,b+n,c,d)
increaseAge'' n xs = map (hoInc n) xs

milfs [] = []
milfs ((a,b,c,d):xs) | b > 30 && b<40 && c=="Female" = a : milfs xs
                     | otherwise = milfs xs

milfs' xs = [a | (a,b,c,d) <- xs, b > 30, b < 40, c == "Female"]

milffilter (a,b,c,d) = b > 30 && b<40 && c=="Female" 

milfs'' xs = map name (myfilter milffilter xs)

howOld n [] = -1 --not in database
howOld n ((a,b,c,d):xs) | map toLower n == map toLower a = b
                        | otherwise = howOld n xs

swapAgeAndName (a,b,c,d) = (b,a,c,d)

sortedByAge xs = map swapAgeAndName $ sort $ map swapAgeAndName xs

sieve (x:xs) = x : [n | n <- sieve xs, n `mod` x /= 0]

isPrime n = null [x | x <- [2..n - 1], n `mod` x == 0]

firstPrimes n = take n [x | x <- [2..], isPrime x]

primesSmallerThan n = [x | x <- [2..n], isPrime x]

dividers n = [x | x <- [1..n], n `mod` x == 0]

isPrime' n = length (dividers n) == 2

pyth n = [(a,b,c) | a <-[2..n], b<-[2..n], c<-[2..n], a**2 + b**2 == c**2]

pyth':: Int -> [(Int,Int,Int)]
pyth' x = [(a,b,c)|a<-[1..x], b<-[1..x], c<-[1..x], (a^2)+(b^2) == (c^2), gcd a b == 1, b>a]

increasing:: [Int] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:x':xs) | x < x' = increasing (x':xs)
                     | otherwise = False

-- Average function taken from https://stackoverflow.com/questions/2376981/haskell-types-frustrating-a-simple-average-function
average:: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

weaklyIncreasing:: [Double] -> Bool
weaklyIncreasing [] = True
weaklyIncreasing [x] = True
weaklyIncreasing xs = (average (init xs) < last xs) && (weaklyIncreasing $ init xs)

sublist:: (Eq a) => [a] -> [a] -> Bool
sublist xs [] = False
sublist xs (y:ys) | xs == take (length xs) (y:ys) = True
                  | otherwise = sublist xs ys 

partialsublist:: (Eq a) => [a] -> [a] -> Bool
partialsublist [] _ = True
partialsublist xs [] = False 
partialsublist (x:xs) (y:ys) | x == y = partialsublist xs ys
                             | otherwise = partialsublist (x:xs) ys

bubble:: (Ord a) => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:x':xs) | x<=x'       = x  : bubble (x':xs)
                 | otherwise   = x' : bubble (x:xs)

bsort:: (Ord a) => [a] -> [a]
bsort [] = []
bsort xs = bsort (init ys) ++ [last ys]
       where
           ys = bubble xs

prop_bsort:: [Int] -> Bool
prop_bsort xs = bsort xs == sort xs

mmsort:: (Ord a) => [a] -> [a]
mmsort [] = []
mmsort [x] = [x]
mmsort xs = mn : mmsort (xs \\ [mn,mx]) ++ [mx]
             where
                mn = minimum xs
                mx = maximum xs

prop_mmsort:: [Int] -> Bool
prop_mmsort xs = mmsort xs == sort xs

ins:: (Ord a) => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys) | x <= y = x : y : ys
             | otherwise = y : ins x ys

isort:: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = ins x (isort xs)

prop_isort:: [Int] -> Bool
prop_isort xs = isort xs == sort xs

merge:: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort:: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst xs)) (msort (snd xs))
        where 
            fst xs = take (length xs `div`2) xs
            snd xs = drop (length xs `div`2) xs

prop_msort:: [Int] -> Bool
prop_msort xs = msort xs == sort xs
 
qsort::(Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

prop_qsort:: [Int] -> Bool
prop_qsort xs = qsort xs == sort xs


