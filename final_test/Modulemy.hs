module Modulemy where
import System.Directory (doesFileExist)
import Data.List

ex1 :: Int -> [a] -> [[a]]
ex1 n xs
    | n <= 0    = error "Chunk size must be greater than 0"
    | null xs   = []
    | otherwise = take n xs : ex1 n (drop n xs)

ex2 :: [[Int]]
ex2 =
    let triplesWithSum = [ [a,b,c,s] |
                           m <- [2..20], n <- [1..(m-1)],
                           let a0 = m^2 - n^2,
                           let b0 = 2*m*n,
                           let c0 = m^2 + n^2,
                           k <- [1..10],
                           let a = k * a0,
                           let b = k * b0,
                           let c = k * c0,
                           a < b, b < c,
                           even a, even b, even c,
                           a >= 1, b >= 1, c >= 1,
                           a <= 200, b <= 200, c <= 200,
                           let s = a + b + c
                         ]
        groupedTriples = groupBy (\t1 t2 -> last t1 == last t2)
                          $ sortBy (\t1 t2 -> compare (last t1) (last t2))
                          triplesWithSum
        finalTriples = concat [ if length grp > 1 then grp else [] | grp <- groupedTriples ]
    in finalTriples

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
