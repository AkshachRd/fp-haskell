import GHC.IO.Encoding

-- Make a list of length N starting from number N
do_my_list n = take n [n..]

-- Swap two sibling elements of the list
oddEven [] = []
oddEven [x] = [x]
oddEven (x:y:xs) = y:x:oddEven xs

-- Insert an element inside the list on given position
insert [] a n = [a]
insert ls a 0 = a:ls
insert (x:ls) a n = x:insert ls a (n - 1)

-- Make a list of sums of elements from two lists
listSumm [] [] = []
listSumm (a:la) [] = a:listSumm la []
listSumm [] (b:lb) = b:listSumm [] lb
listSumm (a:la) (b:lb) = (a + b):listSumm la lb

-- Find the first occurrence of element in the list
position ls a   | not (null filteredList) = head filteredList
                | otherwise = -1
                where   filteredList = filter ((== a) . (ls !!)) indecies
                        indecies = [0..length ls - 1]

-- Find the sum of i where i=1 to n, n is provided
findSumm1 n | n == 1 = 1
            | n < 1 = 0
            | otherwise = n + findSumm1 (n - 1)

-- Find the sum of n-i where i=1 to n, n is provided
findSumm2 n = sum [n - x | x <- [1..n]]

main = do
    setLocaleEncoding utf8

    -- 0)
    putStrLn "0) Make a list of length N starting from number N:"
    let x = 4
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (do_my_list x) ++ "\n"

    let x = 5
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (do_my_list x) ++ "\n"

    -- 1)
    putStrLn "1) Swap two sibling elements of the list:"
    let x = [1]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (oddEven x) ++ "\n"

    let x = [1, 2]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (oddEven x) ++ "\n"

    let x = [1, 2, 3]
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (oddEven x) ++ "\n"

    -- 2)
    putStrLn "2) Insert an element inside the list on given position:"
    let x = [1, 2, 3]
    let y = 5
    let z = 2
    putStrLn  $ "Entering: " ++ show x ++ " " ++ show y ++ " " ++ show z
    putStrLn  $ "Answer: " ++ show (insert x y z) ++ "\n"

    let x = []
    let y = 5
    let z = 2
    putStrLn  $ "Entering: " ++ "[]" ++ " " ++ show y ++ " " ++ show z
    putStrLn  $ "Answer: " ++ show (insert x y z) ++ "\n"

    let x = [1]
    let y = 5
    let z = 10
    putStrLn  $ "Entering: " ++ show x ++ " " ++ show y ++ " " ++ show z
    putStrLn  $ "Answer: " ++ show (insert x y z) ++ "\n"

    -- 3)
    putStrLn "3) Make a list of sums of elements from two lists:"
    let x = [1, 2, 3]
    let y = []
    putStrLn  $ "Entering: " ++ show x ++ " []"
    putStrLn  $ "Answer: " ++ show (listSumm x y) ++ "\n"

    let x = [1, 2, 3]
    let y = [1, 2, 3, 4]
    putStrLn  $ "Entering: " ++ show x ++ " " ++ show y
    putStrLn  $ "Answer: " ++ show (listSumm x y) ++ "\n"

    let x = [1, 2, 3]
    let y = [4, 5, 6]
    putStrLn  $ "Entering: " ++ show x ++ " " ++ show y
    putStrLn  $ "Answer: " ++ show (listSumm x y) ++ "\n"

    -- 4)
    putStrLn "3) Find the first occurrence of element in the list:"
    let x = [1, 2, 3]
    let y = 2
    putStrLn  $ "Entering: " ++ show x ++ " " ++ show y
    putStrLn  $ "Answer: " ++ show (position x y) ++ "\n"

    let x = [1, 2, 3]
    let y = 0
    putStrLn  $ "Entering: " ++ show x ++ " " ++ show y
    putStrLn  $ "Answer: " ++ show (position x y) ++ "\n"

    let x = []
    let y = 1
    putStrLn  $ "Entering: " ++ "[]" ++ " " ++ show y
    putStrLn  $ "Answer: " ++ show (position x y) ++ "\n"

    -- 5)
    putStrLn "5) Find the sum of i where i=1 to n, n is provided:"
    let x = 10
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (findSumm1 x) ++ "\n"

    let x = 0
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (findSumm1 x) ++ "\n"

    let x = -1
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (findSumm1 x) ++ "\n"

    -- 6)
    putStrLn "6) Find the sum of n-i where i=1 to n, n is provided:"
    let x = 10
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (findSumm2 x) ++ "\n"

    let x = 0
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (findSumm2 x) ++ "\n"

    let x = -1
    putStrLn  $ "Entering: " ++ show x
    putStrLn  $ "Answer: " ++ show (findSumm2 x) ++ "\n"
