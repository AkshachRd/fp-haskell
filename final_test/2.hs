-- 2.hs
import Modulemy (ex1, ex2, ex3, ex4)

-- Test cases for ex1
ex_1_tests = do
    putStrLn "Testing ex1:"
    -- Test case 1: Standard usage with exact multiple of n
    print $ ex1 2 [1,2,3,4]        -- Expected: [[1,2],[3,4]]
    -- Test case 2: Last chunk has fewer elements than n
    print $ ex1 3 [1,2,3,4,5,6,7]  -- Expected: [[1,2,3],[4,5,6],[7]]
    -- Test case 3: n is 1 (each element in its own sublist)
    print $ ex1 1 [1,2,3]          -- Expected: [[1],[2],[3]]
    -- Test case 4: n is larger than the list length
    print $ ex1 5 [1,2]            -- Expected: [[1,2]]
    -- Test case 5: Empty list
    print $ ex1 3 ([]::[Int])              -- Expected: []
    -- Test case 6: n is zero (should raise an error)
    -- Uncomment the following line to see the error
    -- print $ ex1 0 [1,2,3]

-- Test cases for ex2
ex_2_tests = do
    putStrLn "\nTesting ex2:"
    let result = ex2
    -- Printing all the Pythagorean triples found
    mapM_ print result
    -- Additional checks can be performed to verify the correctness of the output
    -- For the purpose of this test, we are displaying the results

-- Test cases for ex3
ex_3_tests = do
    putStrLn "\nTesting ex3:"
    -- Test case 1: Some elements match
    print $ ex3 [1,2,3,4,5] [1,0,3,0,5]       -- Expected: [0,2,4]
    -- Test case 2: No elements match
    print $ ex3 [1,2,3] [4,5,6]               -- Expected: []
    -- Test case 3: All elements match
    print $ ex3 [7,8,9] [7,8,9]               -- Expected: [0,1,2]
    -- Test case 4: Lists of different lengths
    print $ ex3 [1,2,3,4,5] [1,2,3]           -- Expected: [0,1,2]
    -- Test case 5: Empty lists
    print $ ex3 ([]::[Int])  ([]::[Int])                    -- Expected: []

-- Test cases for ex4
ex_4_tests = do
    putStrLn "\nTesting ex4:"
    -- Create a temporary input file with test data
    let testInput = unlines
            [ "Line 1"
            , "Line 2"
            , "Line 1"   -- Duplicate
            , ""
            , "Line 3"
            , "Line 2"   -- Duplicate
            , "Line 4"
            , ""
            , "Line 5"
            ]
    writeFile "test_input.txt" testInput
    -- Run ex4 on the test input file
    ex4 "test_input.txt" "test_output.txt"
    -- Read and print the contents of the output file
    putStrLn "Contents of test_output.txt:"
    contents <- readFile "test_output.txt"
    putStrLn contents
    -- Expected output:
    -- Line 3
    -- Line 4
    -- Line 5
    -- (unique lines only)

main = do
    putStrLn "Enter the function to test (ex1, ex2, ex3, ex4):"
    testChoice <- getLine
    case testChoice of
        "ex1" -> ex_1_tests
        "ex2" -> ex_2_tests
        "ex3" -> ex_3_tests
        "ex4" -> ex_4_tests
        _     -> putStrLn "Invalid choice. Please enter ex1, ex2, ex3, or ex4."
