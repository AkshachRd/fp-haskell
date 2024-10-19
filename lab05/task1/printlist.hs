main :: IO ()
main = do
    putStrLn "Введите начальное значение:"
    startValue <- readLn
    putStrLn "Введите количество элементов:"
    count <- readLn
    putStrLn "Введите кратность:"
    multiple <- readLn
    let adjustedStart = if startValue `mod` multiple == 0 then startValue else startValue + (multiple - (startValue `mod` multiple))
    let result = take count [adjustedStart, adjustedStart + multiple..]
    print result
