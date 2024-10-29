module Modulemy where
import System.Directory (doesFileExist)
import Data.List

ex1 :: Int -> [a] -> [[a]]
ex1 n xs
    | n <= 0    = error "Chunk size must be greater than 0"
    | null xs   = []
    | otherwise = take n xs : ex1 n (drop n xs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

generatePythagoreanTriples :: [(Int, Int, Int)]
generatePythagoreanTriples = [
    (a, b, c) |
        m <- [2..100],                -- m and n are positive integers with m > n
        n <- [1..(m - 1)],
        let a0 = m^2 - n^2,           -- Euclid's formula to generate primitive triples
        let b0 = 2 * m * n,
        let c0 = m^2 + n^2,
        k <- [1..100],                -- Scaling factor k to generate multiples of the primitive triple
        let a = k * a0,               -- Scale the sides
        let b = k * b0,
        let c = k * c0,
        a < b, b < c,                 -- Ensure the sides are in ascending order
        even a, even b, even c,       -- Only consider even numbers
        a >= 2, b >= 2, c >= 2,       -- Sides must be at least 2
        a <= 200, b <= 200, c <= 200  -- Sides must be at most 200
    ]

triplesWithSums :: [(Int, Int, Int, Int)]
triplesWithSums = [
    (a, b, c, sumABC) |
        (a, b, c) <- generatePythagoreanTriples,
        let sumABC = a + b + c
    ]

filteredTriples :: [(Int, Int, Int, Int)]
filteredTriples = removeDuplicates triplesWithSums

sortedBySum :: [(Int, Int, Int, Int)]
sortedBySum =
    sortBy (\(_, _, _, sum1) (_, _, _, sum2) -> compare sum1 sum2) filteredTriples

ex2 :: [[Int]]
ex2 = [
    [a, b, c, sumABC] |
    (a, b, c, sumABC) <- sortedBySum
    ]

ex3 :: Eq a => [a] -> [a] -> [Int]
ex3 xs ys = [i | (i, (x, y)) <- pairsWithIndex, x == y]
    where pairs = zip xs ys
          pairsWithIndex = zip [0..] pairs

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (== x)


ex4 :: FilePath -> FilePath -> IO ()
ex4 inputFileName outputFileName = do
    inputFileExists <- doesFileExist inputFileName
    if not inputFileExists then putStrLn "error"
    else do
        content <- readFile inputFileName
        let linesList = lines content
        if null linesList then putStrLn "error"
        else do
            let notUniqueLines = [line | line <- linesList, countOccurrences line linesList > 1]
            let outputContent = (unlines . removeDuplicates) notUniqueLines
            writeFile outputFileName outputContent
