module Modulemy where
import System.Directory (doesFileExist)
import Data.List

ex1 :: Int -> [a] -> [[a]]
ex1 n xs
    | n <= 0    = error "Chunk size must be greater than 0"
    | null xs   = []
    | otherwise = take n xs : ex1 n (drop n xs)

-- Step 1: Generate Pythagorean triples using Euclid's formula
generatePythagoreanTriples :: [(Int, Int, Int)]
generatePythagoreanTriples = [
    (a, b, c) |
        m <- [2..20],                 -- m and n are positive integers with m > n
        n <- [1..(m - 1)],
        let a0 = m^2 - n^2,           -- Euclid's formula to generate primitive triples
        let b0 = 2 * m * n,
        let c0 = m^2 + n^2,
        k <- [1..10],                 -- Scaling factor k to generate multiples of the primitive triple
        let a = k * a0,               -- Scale the sides
        let b = k * b0,
        let c = k * c0,
        a < b, b < c,                 -- Ensure the sides are in ascending order
        even a, even b, even c,       -- Only consider even numbers
        a >= 2, b >= 2, c >= 2,       -- Sides must be at least 2
        a <= 200, b <= 200, c <= 200  -- Sides must be at most 200
    ]

-- Step 2: Calculate the sum of each triple
triplesWithSums :: [(Int, Int, Int, Int)]
triplesWithSums = [
    (a, b, c, sumABC) |
        (a, b, c) <- generatePythagoreanTriples,
        let sumABC = a + b + c        -- Calculate the sum of the sides
    ]

-- Step 3: Group the triples by their sums
groupedBySum :: [[(Int, Int, Int, Int)]]
groupedBySum =
    groupBy (\(_, _, _, sum1) (_, _, _, sum2) -> sum1 == sum2) $
    sortBy (\(_, _, _, sum1) (_, _, _, sum2) -> compare sum1 sum2) triplesWithSums

-- Step 4: Filter out sums that have only one triple
sumsWithMultipleTriples :: [[(Int, Int, Int, Int)]]
sumsWithMultipleTriples = [
    group |
    group <- groupedBySum,
    length group > 1                 -- Keep only groups with more than one triple
    ]

-- Step 5: Flatten the list of groups into a list of triples
filteredTriples :: [(Int, Int, Int, Int)]
filteredTriples = concat sumsWithMultipleTriples

-- Step 6: Convert each tuple to a list for the final output
finalTriples :: [[Int]]
finalTriples = [
    [a, b, c, sumABC] |
    (a, b, c, sumABC) <- filteredTriples
    ]

ex2 :: [[Int]]
ex2 = finalTriples

ex3 :: Eq a => [a] -> [a] -> [Int]
ex3 xs ys = [i | (i, (x, y)) <- pairsWithIndex, x == y]
    where pairs = zip xs ys
          pairsWithIndex = zip [0..] pairs

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (== x)

ex4 :: FilePath -> FilePath -> IO ()
ex4 inputFileName outputFileName = do
    inputFileExists <- doesFileExist inputFileName
    outputFileExists <- doesFileExist outputFileName

    if not inputFileExists || not outputFileExists then putStrLn "error"
    else do
        content <- readFile inputFileName
        let linesList = lines content
        if null linesList then putStrLn "error"
        else do
            let uniqueLines = [line | line <- linesList, countOccurrences line linesList == 1]
            let outputContent = unlines uniqueLines
            writeFile outputFileName outputContent
