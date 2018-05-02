import Test.QuickCheck
import Data.Char

f:: Integer -> Integer
f x = 2*x^2 + 3 * x - 5

total1 :: Int -> Int
total1 0 = 0 
total1 n = total1 (n-1) + n

total2 :: Int -> Int
total2 n = (n * ( n )) `div` 2

prop_total n = (n >= 0) ==> total1 n == total2 n

prop_plus :: Int -> Int -> Bool
prop_plus x y = x + y == y + x

prop_min :: Int -> Int -> Bool
prop_min x y = x - y == y - x

-- code :: Char -> Char
-- code c | c `myelem` ['a' .. 'z'] = chr ((ord c + 10) `mod` 26 + 97)
--        | ord c >= 65 && ord c <= 90 = chr ((ord c + 16) `mod` 26 + 65)
--        | otherwise = c

-- test1 = map code "hello"
-- test2 = map code "Tomorrow evening, 8 o'clock in Amsterdam"


interest a r 0 = a
interest a r n = interest (a * (1+r/100)) r (n-1)


discr :: Float -> Float -> Float -> Float
discr a b c = b * b - 4 * a * c

root1 :: Float -> Float -> Float -> Float
root1 a b c
    | discr a b c < 0 = error("Negative discriminant")
    | otherwise = ((-1 * b + sqrt(discr a b c)) / (2 * a))

root2 :: Float -> Float -> Float -> Float
root2 a b c
    | discr a b c < 0 = error("Negative discriminant")
    | otherwise = ((-1 * b - sqrt(discr a b c)) / (2 * a))

extrX :: Float -> Float -> Float -> Float
extrX a b c = -b / (2*a)

extrY :: Float -> Float -> Float -> Float
extrY a b c = a * (extrX a b c ** 2) + b * extrX a b c + c

mylength:: [a] -> Int
mylength [] = 0
mylength (x:xs) = mylength xs + 1 

mysum:: (Num a) => [a] -> a
mysum [] = 0
mysum (x:xs) = mysum xs + x

myreverse:: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake:: [a] -> Int -> [a]
mytake [] n = []
mytake _ 0  = []
mytake (x:xs) n = [x] ++ mytake xs (n-1) 

myelem:: (Eq a) => [a] -> a -> Bool
myelem [] a = False
myelem (x:xs) a | x == a    = True
                | otherwise = myelem xs a

myconcat:: [a] -> [a] -> [a]
myconcat xs [] = xs
myconcat xs (y:ys) = myconcat (xs ++ [y]) ys
 
mymaximum:: (Num a, Ord a) => [a] -> a
mymaximum [] = -1
mymaximum [x] = x
mymaximum (x:xs) | x > mymaximum xs = x
                 | otherwise        = mymaximum xs

myzip:: [a] -> [a] -> [(a,a)]
myzip xs [] = []
myzip [] ys = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys

r:: (Num a) => a -> a -> [a]
r a d = [a] ++ r (a+d) d

r1:: (Num a) => a -> a -> Int -> a
r1 a d 0 = a
r1 a d n = r1 (a+d) d (n-1)

totalr:: (Num a) => a -> a -> Int -> Int -> a
totalr a d i j = mysum (map (r1 a d) [i..j])

allEqual:: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:x':xs) | x==x'     = allEqual (x':xs)
                   | otherwise = False

isAS:: (Num a, Eq a) => [a] -> Bool
isAS xs = allEqual (differences xs)

differences:: (Num a, Eq a) => [a] -> [a]
differences [] = []
differences [x] = []
differences (x:x':xs) = [x-x'] ++ differences (x':xs)

rowLengthEqual:: [[a]] -> Bool
rowLengthEqual xs = allEqual (map mylength xs)

rowTotals :: (Num a) => [[a]] -> [a]
rowTotals xs = map mysum xs

mytranspose :: [[a]] -> [[a]]
mytranspose [] = []
mytranspose [x] = transposeRow x
mytranspose (x:xs) = zipWith (++) (transposeRow x) (mytranspose xs)

-- mytranspose' :: [[a]] -> [[a]]
-- mytranspose' xs = map $ head xs ++ mytranspose' $ map $ tail xs 

transposeRow :: [a] -> [[a]]
transposeRow [] = []
transposeRow (x:xs) = [[x]] ++ transposeRow xs

colTotals :: (Num a) => [[a]] -> [a]
colTotals xs = rowTotals (mytranspose xs)
