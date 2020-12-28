{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Map (fromListWith, toList)


----- part 1
f1_1 :: Int -> Int
f1_1 x = x

f2_1 :: Int -> Bool
f2_1 y = (y > 1) && null [x | x <- [2..y], y `mod` x == 0]

f3_1 :: Bool -> Bool -> Int
f3_1 x y = fromEnum x + fromEnum y

f4_1 :: Int -> Int
f4_1 y = sum $ filter (\x -> y `mod` x == 0) [1..y `div` 2 + 1]

f5_1 :: Int -> Int
f5_1 n = if f4_1 (n + 1) == n + 1 then n + 1 
         else f5_1 (n + 1)

f6_1 :: Integer -> Integer
f6_1 y = if sum [x | x <- [1..y + 1], (y +  1) `mod` x == 0, x /= (y + 1)] == (y + 1) then y + 1
         else f6_1 (y + 1)         

f7_1 :: Int -> Int -> Int
f7_1 0 n = n + 1
f7_1 m 0 = f7_1 (m - 1) 1
f7_1 m n = f7_1 (m - 1) (f7_1 m (n - 1))

f8_1 :: Int -> Int -> Integer
f8_1 0 n = toInteger (n + 1)
f8_1 m 0 = f8_1 (m - 1) 1
f8_1 m n = f8_1 (m - 1) (fromInteger (f8_1 m (n - 1)))         


-----Нужно еще 9 сделать

f10_0 :: (Double, Double) -> (Double, Double)
f10_0 (n, m) = (n + 1, m)

f10_1 :: Double -> Double -> (Double, Double)
f10_1 a b = if a < b then (0.0, a) 
            else f10_0 (f10_1 (a - b) b)

f11_0 :: Int -> Int -> Double
f11_0 k 0 = 2
f11_0 k t = 2.0 + (((2.0 * fromIntegral k - 1) ^ 2) / f11_0 (k + 1) (t - 1))

f11_1 :: Int -> Double
f11_1 k = 1 / (f11_0 1 (k - 1) - 1)

f12_1 :: Int
f12_1 = 1 + 1

----------- part 2


antisort :: Ord a => [a] -> [a]
antisort [x, y, z] = [maximum [x, y, z], minimum [x, y, z]] ++ delete (minimum [x, y, z]) (delete (maximum [x, y, z]) [x, y, z])
antisort xs = head xs : antisort (tail xs)

isPrime :: Integer -> Bool
isPrime y = (y > 1) && null [x | x <- [2..y-1], y `mod` x == 0]

antiprimes :: Int -> [Integer]
antiprimes k = take k ([x | x <- [1..], not (isPrime x)])

antiunion :: Eq a => [a] -> [a] -> [a]
antiunion a b = (\\) a b ++ (\\) b a


antimergeInc :: Eq a => a -> [(Int, a)] -> [(Int, a)]
antimergeInc it [] = [(1, it)]
antimergeInc it (hd : list) =
    if snd hd == it then
        (fst hd + 1, it) : list
    else
        hd : antimergeInc it list

antimerge :: Eq a => [a] -> [(Int, a)]
antimerge = foldr antimergeInc [] 

lengthGroup :: [a] -> (Int, a)
lengthGroup xs = (length xs, head xs)

antiintercalate :: Eq a => [a] -> [(Int, a)]
antiintercalate [] = []
antiintercalate xs = map lengthGroup (group xs)

antiantiintercalate :: [(Int, a)] -> [a]
antiantiintercalate [] = []
antiantiintercalate [(cnt, x)] = replicate cnt x
antiantiintercalate xs = antiantiintercalate [head xs] ++ antiantiintercalate (tail xs)

getNumberOrNot0 :: String -> Maybe Integer
getNumberOrNot0 [] = Nothing
getNumberOrNot0 [x] = if isDigit x then Just (toInteger (digitToInt x)) 
                                   else Nothing
getNumberOrNot0 xs = case getNumberOrNot [last xs] of
    Nothing -> Nothing
    Just val -> case getNumberOrNot (init xs) of
        Nothing -> Nothing
        Just v -> Just (val + v * 10)

getNumberOrNot :: String -> Maybe Integer
getNumberOrNot xs = getNumberOrNot0 (filter (not . isSpace) xs)


maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> a -> a
maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot mb df = fromMaybe df (join (join (join (join (join mb)))))


stupidTraverse0 :: [a] -> Maybe [(a, a, a, a)]
stupidTraverse0 [] = Just []
stupidTraverse0 [a] = Nothing
stupidTraverse0 [a, b] = Nothing
stupidTraverse0 [a, b, c] = Nothing
stupidTraverse0 [a, b, c, d] = Just [(a, b, c, d)]
stupidTraverse0 xs = case stupidTraverse0 (drop 4 xs) of
    Nothing -> Nothing
    Just v -> Just (fromJust (stupidTraverse0 (take 4 xs)) ++ v)

-- stupidTraverse [Nothing, Nothing, Nothing, Nothing] - не работает

stupidTraverse :: [Maybe a] -> Maybe [(a, a, a, a)]
stupidTraverse xs = stupidTraverse0 (map fromJust (filter (not . null) xs))

dfs0 :: [(Int, Int)] -> Int -> Int -> [Int] -> Bool
dfs0 edges from to visitedEdges =
    (from == to) || or (map (\ dest -> dfs0 edges (snd dest) to (from : visitedEdges))
            (filter (not . (`elem` visitedEdges) . snd) (filter ((== from). fst) edges)))
                
dfs :: [(Int, Int)] -> Int -> Int -> Bool
dfs edges from to = dfs0 edges from to []

frst :: (a, b, c) -> a
frst (a, b, c) = a

scnd :: (a, b, c) -> b
scnd (a, b, c) = b

thrd :: (a, b, c) -> c
thrd (a, b, c) = c

fatWay0 :: (Num a, Ord a) => [(Int, Int, a)] -> Int -> Int -> [Int] -> [a]
fatWay0 edges from to visitedEdges =
    if from == to then [0]
    else sort (concatMap (\dest -> map (\l -> l + thrd dest) 
    (fatWay0 edges (scnd dest) to (from : visitedEdges)))
            (filter (not . (`elem` visitedEdges) . scnd)
                (filter ((== from) . frst) edges)))

fatWay :: (Num a, Ord a) => [(Int, Int, a)] -> Int -> Int -> a
fatWay edges from to = case listToMaybe (take 1 (drop 1 (fatWay0 edges from to []))) of
    Nothing -> fromInteger (negate 1)
    Just val -> val



------ part 3

data Tree a = EmptyTree
            | TreeValue a (Tree a) (Tree a) (Tree a)
            deriving Show

justValue :: a -> Tree a
justValue val = TreeValue val EmptyTree EmptyTree EmptyTree

seek :: Ord a => a -> Tree a -> Maybe a
seek val EmptyTree = Nothing
seek val (TreeValue a _left _center _right) =
    if val == a then Just a
    else 
        case seek val _left of
        Just _leftA -> Just _leftA
        Nothing -> case seek val _center of
            Just centerA -> Just centerA
            Nothing -> seek val _right

addFarLeft :: Ord a => Tree a -> Tree a -> Tree a
addFarLeft _subtree EmptyTree = _subtree
addFarLeft _subtree (TreeValue a _left _center _right) = TreeValue a (addFarLeft _subtree _left) _center _right

addFarRight :: Ord a => Tree a -> Tree a -> Tree a
addFarRight _subtree EmptyTree = _subtree
addFarRight _subtree (TreeValue a _left _center _right) = TreeValue a _left _center (addFarLeft _subtree _right)

deleteTree :: Ord a => a -> Tree a -> Tree a
deleteTree val EmptyTree = EmptyTree
deleteTree val (TreeValue a _left _center _right) =
    if val == a then addFarRight _right (addFarLeft _left _center)
    else TreeValue a
        (deleteTree val _left)
        (deleteTree val _center)
        (deleteTree val _right)

maxTree :: Ord a => Tree a -> Maybe a
maxTree EmptyTree = Nothing
maxTree (TreeValue a _ _center _right) =
    case maxTree _right of
        Just rightMax -> Just rightMax
        Nothing -> case maxTree _center of
            Just centerMax -> Just (max centerMax a)
            Nothing -> Just a

addTree :: Ord a => a -> Tree a -> Tree a
addTree val EmptyTree = justValue val
addTree val (TreeValue a _left _center _right)
  | val < a
  = case maxTree _left of
      Just _leftMax
        -> if _leftMax > val then
               TreeValue a (addTree val _left) _center _right
           else
               TreeValue a _left (addTree val _center) _right
      Nothing -> TreeValue a (addTree val _left) _center _right
  | val > a
  = case maxTree _center of
      Just centerMax
        -> if centerMax > val then
               TreeValue a _left (addTree val _center) _right
           else
               TreeValue a _left _center (addTree val _right)
      Nothing -> TreeValue a _left _center (addTree val _right)
  | otherwise = TreeValue a _left _center _right


insertAsc :: Ord a => a -> [a] -> [a]
insertAsc x [] = [x]
insertAsc x (y:ys) =
    if x <= y then x : y: ys
    else y : insert x ys

kmin :: Ord a => Int -> Tree a -> [a]
kmin 0 _ = []
kmin _ EmptyTree = []
kmin k (TreeValue a _left _center _right) = take k (kmin k _left ++ insertAsc a (kmin k _center) ++ kmin k _right)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (TreeValue a _left _center _right) =
        TreeValue (f a) (fmap f _left) (fmap f _center) (fmap f _right)

data N = Z | S N deriving Show

instance Eq N where
    (==) Z Z = True
    (==) (S _) Z = False
    (==) Z (S _) = False
    (==) (S x) (S y) = x == y

instance Ord N where
    compare Z Z = EQ
    compare (S _) Z = GT
    compare Z (S _) = LT
    compare (S x) (S y) = compare x y

natSum :: N -> N -> N
natSum Z x = x
natSum x Z = x
natSum x (S y) = S (natSum x y)

natMult :: N -> N -> N
natMult Z x = Z
natMult x Z = Z
natMult x (S y) = natSum x (natMult x y)

natPow :: N -> N -> N
natPow x Z = S Z
natPow x (S p) = natMult x (natPow x p)

superNat :: N -> N -> N -> N
superNat Z _ b = S b
superNat (S Z) a Z = a
superNat (S (S Z)) a Z = Z
superNat n a Z = S Z
superNat (S n) a (S b) = superNat n a (superNat (S n) a b)


getInt :: IO Integer
getInt = readLn

apb :: IO ()
apb = do
    a <- getInt
    b <- getInt
    print (a + b)

whileNotZero :: IO ()
whileNotZero = do
    a <- getInt
    unless (a == 0) whileNotZero

next :: Integer -> Integer
next seed = (1103515245 * seed + 12345) `mod` 1073741824

fakeRandom :: IO()
fakeRandom = do
      seed <- readInt
      k <- readInt 
      print(take k $ iterate next seed)


data FS = File String String
        | Directory String [FS]
        deriving Show

fileSystemName :: FS -> String
fileSystemName (File name _) = name
fileSystemName (Directory name _) = name

fileSystemsAsList :: FS -> String
fileSystemsAsList (Directory name children) = concatMap (\x -> fileSystemName x ++ "\n") children

fileSystemAsTree :: FS -> String -> String
fileSystemAsTree (File name _) prefix =
    prefix ++ " " ++ name ++ "\n"
fileSystemAsTree (Directory name children) prefix =
    prefix ++ "> " ++ name ++ "\n" ++ concatMap (\c -> fileSystemAsTree c ('|' : prefix)) children

fileSystemFind :: FS -> String -> Maybe FS
fileSystemFind (File _ _) _ = Nothing
fileSystemFind (Directory _ children) req = find (\c -> fileSystemName c == req) children

fileSystemFindDir :: FS -> String -> Maybe FS
fileSystemFindDir (File _ _) _ = Nothing
fileSystemFindDir (Directory _ children) cd = find (\case
  (File _ _) -> False
  (Directory name _) -> (name == cd)) children

fileSystemCreateDir :: FS -> String -> FS
fileSystemCreateDir (Directory name children) newName =
    Directory name (Directory newName [] : children)

fileSystemCreateFile :: FS -> String -> String -> FS
fileSystemCreateFile (Directory name children) newName newContents =
    Directory name (File newName newContents : children)

fileSystemReplace :: FS -> FS -> FS
fileSystemReplace (Directory name children) updatedSubDir =
    Directory name (map
        (\it -> case it of
            (File _ _) -> it
            (Directory name _) -> if name == fileSystemName updatedSubDir then updatedSubDir else it
        ) children)

fileSystemSplitNameAndContents0 :: (String, String) -> Char -> (String, String)
fileSystemSplitNameAndContents0 (prev1, prev2) c = (c:prev1, prev2)

fileSystemSplitNameAndContents :: String -> (String, String)
fileSystemSplitNameAndContents "" = ("", "")
fileSystemSplitNameAndContents (' ':tl) = ("", tl)
fileSystemSplitNameAndContents str = fileSystemSplitNameAndContents0 (fileSystemSplitNameAndContents (tail str)) (head str)


runFS0 :: [FS] -> FS -> IO ()
runFS0 parents fs = do 
    line <- getLine
    case line of
        "exit" -> return ()
        "ls" -> do
            putStrLn (fileSystemsAsList fs)
            runFS0 parents fs
        "asTree" -> do
            putStrLn (fileSystemAsTree fs "")
            runFS0 parents fs
        ('c':'d':' ': dirName) ->
            if dirName == ".." then
                runFS0 (tail parents) (fileSystemReplace (head parents) fs)
            else
                case fileSystemFindDir fs dirName of
                    Nothing -> do
                        putStrLn ("directory " ++ dirName ++ " doesn't exist")
                        runFS0 parents fs
                    Just dir -> do
                        runFS0 (fs : parents) dir
        ('m':'k':'d':'i':'r':' ': dirName) ->
            if isNothing (fileSystemFind fs dirName) then
                runFS0 parents (fileSystemCreateDir fs dirName)
            else do
                putStrLn ("item " ++ dirName ++ " already exists")
                runFS0 parents fs
        ('t':'o':'u':'c':'h':' ': fileNameAndContents) ->
            if isNothing (fileSystemFind fs (fst (fileSystemSplitNameAndContents fileNameAndContents))) then
                runFS0 parents (uncurry (fileSystemCreateFile fs) (fileSystemSplitNameAndContents fileNameAndContents))
            else do
                putStrLn ("item " ++ fst (fileSystemSplitNameAndContents fileNameAndContents) ++ " already exists")
                runFS0 parents fs
        unknown -> do
            putStrLn ("command " ++ unknown ++ " doesn't exist")
            runFS0 parents fs

runFS :: FS -> IO ()
runFS fs = do
    runFS0 [] fs
    return ()
    
part1 = do
    print "Task 1"
    print (f1_1 8)

    print "Task 2"
    print (f2_1 3)
    print (f2_1 (-3))
    print (f2_1 6)

    print "Task 3"
    print (f3_1 False False)
    print (f3_1 True False)
    print (f3_1 True True)

    print "Task 4"
    print (f4_1 5)
    print (f4_1 6)
    print (f4_1 (-6))

    print "Task 5"
    print (f5_1 5)
    print (f5_1 28)

    print "Task 6"
    print (f6_1 5)
    print (f6_1 28)

    print "Task 7"
    print (f7_1 0 0)
    print (f7_1 3 4)

    print "Task 8"
    print (f8_1 0 0)
    print (f8_1 3 4)

    print "Task 10"
    print (f10_1 6.0 2.7)

    print "Task 11"
    print (f11_1 5)
    print (f11_1 6)
    print (f11_1 7)

    print "Task 12"
    print (f12_1 == 1 + 1)


part2 = do
    print "Task 1"
    print (antisort [1, 2, 3])
    print (antisort [1, 2, 3, 4, 7])

    print "Task 2"
    print (antiprimes 5)
    print (antiprimes 10)

    print "Task 3"
    print (antiunion [1, 2, 3] [2, 3, 4])
    print (antiunion [1, 2] [5, 7])
    print (antiunion [1, 2] [1, 2])

    print "Task 4"
    print (antimerge [1, 2, 1, 2, 1])
    print (antimerge [2, 3, 4, 3, 5, 7])

    print "Task 5"
    print (antiintercalate [1, 1, 1, 2, 2, 1, 1, 1, 3])

    print "Task 6"
    print (antiantiintercalate [(3, 3), (2, 1), (3, 2), (1, 3)])

    print "Task 7"
    print (getNumberOrNot "1bs")
    print (getNumberOrNot "123")
    print (getNumberOrNot " 123 456 ")

    print "Task 9"
    print (maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot Nothing 1)

    print "Task 10"
    print (stupidTraverse [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 7])
    print (stupidTraverse [Nothing, Nothing, Just 8, Just 8, Just 8, Just 8, Just 8, Just 8, Just 8, Just 8])
    print (stupidTraverse [Nothing, Just 8, Just 8, Just 8, Just 8])

    print "Task 11"
    print (dfs [(1, 2), (2, 3)] 1 1)
    print (dfs [(1, 2), (2, 3)] 1 2)
    print (dfs [(1, 2), (2, 3)] 2 1)
    print (dfs [(1, 2), (2, 3)] 1 4)

    print "Task 12"
    print (fatWay [(1, 2, 1), (2, 3, 2), (1, 3, 7)] 1 3)
    print (fatWay [(1, 2, 1), (2, 3, 1)] 1 3)
    print (fatWay [(1, 2, 1)] 1 7)


part3 = do
    print "Task 2"
    print (seek 2 EmptyTree)
    print (seek 3 (justValue 2))
    print (seek 2 (justValue 2))
    print (seek 1 (TreeValue 2 (justValue 7) EmptyTree (justValue 1)))

    print "Task 3"
    print (deleteTree 2 (justValue 2))
    print (deleteTree 10 EmptyTree)
    print (deleteTree 9 (TreeValue 2 (justValue 1) EmptyTree (justValue 9)))
    print (deleteTree 2 (TreeValue 2 (justValue 1) EmptyTree (justValue 9)))

    print "Task 4"
    print (addTree 2 EmptyTree)
    print (addTree 10 (justValue 1))
    print (addTree 3 (addTree 1 (addTree 10 (justValue 7))))

    print "Task 8"
    print (natSum (S Z) (S Z))
    print (natSum (S Z) Z)
    print (natSum (S (S Z)) (S (S Z)))

    print "Task 9"
    print (natMult (S (S Z)) (S (S (S Z))))
    print (natMult (S Z) (S Z))

    print "Task 10"
    print (superNat Z (S (S (S Z))) (S (S Z)))
    print (superNat (S Z) (S (S (S Z))) (S (S Z)))
    print (superNat (S (S Z)) (S (S (S Z))) (S (S Z)))

    print "Task 11"
    print (fmap (+ 1) (justValue 3))
    print (fmap (+ 1) (TreeValue 2 (justValue 1) EmptyTree (justValue 9)))

    print "Task 12"
    print (S (S Z) == S (S Z))
    print (Z == Z)
    print (S (S Z) == S Z)
    




part4 = do
    print "Task 1, input two integers: "
    apb
    print "Task 2, input integers until 0: "
    whileNotZero
    print "Task 3, input two integers: "
    fakeRandom
    print "Task 4"
    runFS (Directory "Homework" [File "hw_1" "smth", File "hw_2" "smth", Directory "labs" [File "haskell" "print \"Hello, @TheLesbianManGirl!\""]])


main = do
    print "Hello, @TheLesbianManGirl!"
    print "part 1"
 --   part1
    print "part 2"
 --   part2
    print "part 3"
 --   part3
    print "part 4"
 --   part4
