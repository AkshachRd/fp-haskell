-- Takes N and makes a list of numbers from N to 1
listnums :: Int -> [Int]
listnums n
    | n <= 0    = []
    | otherwise = n : listnums (n - 1)

-- Takes a list of list and returns a new list of the last elements of each sublist
secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist [[]] = []
secondlastlist (x:xs) = last x : secondlastlist xs

-- Concatenates two lists without duplicates
myunion :: Eq a => [a] -> [a] -> [a]
myunion [] ys = ys
myunion (x:xs) ys
    | x `elem` ys = myunion xs ys
    | otherwise   = x : myunion xs ys

-- Takes two lists and returns a new list of elements from the first list that hasn't occured in the second list
mysubst :: Eq a => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst (x:xs) ys
    | x `notElem` ys = x:mysubst xs ys
    | otherwise = mysubst xs ys

-- Takes an index and a list of lists, and returns a new list of the elements at the given index in each sublist
nposlist :: Int -> [[a]] -> [a]
nposlist n lists = (map (!! n) . filter (not . null)) lists

main = do
    -- 1)
    putStrLn "1) Takes N and makes a list of numbers from N to 1:"
    let x = -1
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (listnums x) ++ "\n"

    let x = 0
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (listnums x) ++ "\n"

    let x = 1
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (listnums x) ++ "\n"

    let x = 5
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (listnums x) ++ "\n"

    -- 2)
    putStrLn "2) Takes a list of list and returns a new list of the last elements of each sublist:"
    let x = [] :: [[Int]]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (secondlastlist x) ++ "\n"

    let x = [[]] :: [[Int]]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (secondlastlist x) ++ "\n"

    let x = [[1]]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (secondlastlist x) ++ "\n"

    let x = [[1, 2]]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (secondlastlist x) ++ "\n"

    let x = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (secondlastlist x) ++ "\n"

    -- 3)
    putStrLn "3) Concatenates two lists without duplicates:"
    let x1 = [] :: [Int]
    let y1 = [] :: [Int]
    putStrLn $ "Entering: " ++ show x1 ++ " and " ++ show y1
    putStrLn $ "Answer: " ++ show (myunion x1 y1) ++ "\n"

    let x2 = [1, 2, 3]
    let y2 = [] :: [Int]
    putStrLn $ "Entering: " ++ show x2 ++ " and " ++ show y2
    putStrLn $ "Answer: " ++ show (myunion x2 y2) ++ "\n"

    let x3 = [] :: [Int]
    let y3 = [4, 5, 6]
    putStrLn $ "Entering: " ++ show x3 ++ " and " ++ show y3
    putStrLn $ "Answer: " ++ show (myunion x3 y3) ++ "\n"

    let x4 = [1, 2, 3]
    let y4 = [3, 4, 5]
    putStrLn $ "Entering: " ++ show x4 ++ " and " ++ show y4
    putStrLn $ "Answer: " ++ show (myunion x4 y4) ++ "\n"

    let x7 = [1, 2, 3, 4, 5]
    let y7 = [5, 4, 3, 2, 1]
    putStrLn $ "Entering: " ++ show x7 ++ " and " ++ show y7
    putStrLn $ "Answer: " ++ show (myunion x7 y7) ++ "\n"

    -- 4)
    putStrLn "4) Takes two lists and returns a new list of elements from the first list that hasn't occured in the second list:"
    let x1 = [] :: [Int]
    let y1 = [] :: [Int]
    putStrLn $ "Entering: " ++ show x1 ++ " and " ++ show y1
    putStrLn $ "Answer: " ++ show (mysubst x1 y1) ++ "\n"

    let x2 = [1, 2, 3]
    let y2 = [] :: [Int]
    putStrLn $ "Entering: " ++ show x2 ++ " and " ++ show y2
    putStrLn $ "Answer: " ++ show (mysubst x2 y2) ++ "\n"

    let x3 = [] :: [Int]
    let y3 = [4, 5, 6]
    putStrLn $ "Entering: " ++ show x3 ++ " and " ++ show y3
    putStrLn $ "Answer: " ++ show (mysubst x3 y3) ++ "\n"

    let x4 = [1, 2, 3]
    let y4 = [3, 4, 5]
    putStrLn $ "Entering: " ++ show x4 ++ " and " ++ show y4
    putStrLn $ "Answer: " ++ show (mysubst x4 y4) ++ "\n"

    -- 5)
    putStrLn "5) Takes an index and a list of lists, and returns a new list of the elements at the given index in each sublist:"
    let n1 = 0
    let x1 = [] :: [[Int]]
    putStrLn $ "Entering: " ++ show n1 ++ " and " ++ show x1
    putStrLn $ "Answer: " ++ show (nposlist n1 x1) ++ "\n"

    let n2 = 0
    let x2 = [[]] :: [[Int]]
    putStrLn $ "Entering: " ++ show n2 ++ " and " ++ show x2
    putStrLn $ "Answer: " ++ show (nposlist n2 x2) ++ "\n"

    let n3 = 0
    let x3 = [[1]]
    putStrLn $ "Entering: " ++ show n3 ++ " and " ++ show x3
    putStrLn $ "Answer: " ++ show (nposlist n3 x3) ++ "\n"

    let n4 = 1
    let x4 = [[1, 2]]
    putStrLn $ "Entering: " ++ show n4 ++ " and " ++ show x4
    putStrLn $ "Answer: " ++ show (nposlist n4 x4) ++ "\n"

    let n5 = 2
    let x5 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    putStrLn $ "Entering: " ++ show n5 ++ " and " ++ show x5
    putStrLn $ "Answer: " ++ show (nposlist n5 x5) ++ "\n"
