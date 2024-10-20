import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO
import Data.List (delete, isInfixOf)
import Data.Char(isDigit)

viewFile :: String -> IO ()
viewFile inFileName = do
    content <- readFile inFileName
    putStrLn $ "Файл " ++ show inFileName ++ " содержит:"
    putStrLn content

appendDataFile :: String -> String -> IO ()
appendDataFile inFileName newContent = do
    appendFile inFileName (newContent ++ "\n")
    putStrLn $ "В файл " ++ show inFileName ++ " была добавлена строка:" ++ show newContent

removeLineFromFile :: String -> Int -> IO ()
removeLineFromFile inFileName lineNumber = do
    if lineNumber <= 0
    then putStrLn "Недопустимый номер строки"
    else do
        input <- readFile inFileName
        let lineList = lines input
        if length lineList < lineNumber
        then putStrLn "Номер строки привышает количество строк в файле"
        else do
            let line = lineList !! (lineNumber - 1)
            let output = unlines $ delete line lineList
            writeFile inFileName output
            putStrLn $ "Из файла " ++ show inFileName ++ " была удалена строка с номером:" ++ show lineNumber

copyFileWithBlankLinesFilter :: String -> String -> IO ()
copyFileWithBlankLinesFilter inFileName outputFileName = do
    input <- readFile inFileName
    let output = unlines . filter (not . null) . lines $ input
    writeFile outputFileName output
    putStrLn $ "Строки из файла " ++ show inFileName ++ ", которые не являются пустыми, скопированы в файл: " ++ show outputFileName


copyFileWithKeyWordFilter :: String -> String -> String -> IO ()
copyFileWithKeyWordFilter inFileName outputFileName keyword  = do
    input <- readFile inFileName
    let output = unlines . filter (isInfixOf keyword) . lines $ input
    writeFile outputFileName output
    putStrLn $ "Строки из файла" ++ show inFileName ++ ", в которых есть ключевое слово " ++ keyword ++ ", скопированы в файл:" ++ show outputFileName

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
    then putStrLn "Specify the file name"
    else do
        let [inputFileName] = args
        inputFileExists <- doesFileExist inputFileName
        if not inputFileExists
        then putStrLn $ "File " ++ inputFileName ++ " does not exist"
        else do
            putStrLn "Выберите операцию: 1 - Просмотр, 2 - Добавление, 3 - Удаление, 4 - Копирование с фльтрацией пустых строк"
            operation <- getLine
            case (operation) of
                "1" -> viewFile inputFileName
                "2" -> do
                    putStrLn "Введите строку, которую хотите добавить: "
                    newContent <- getLine
                    appendDataFile inputFileName newContent
                "3" -> do
                    putStrLn "Введите номер строки, которую хотите удалить: "
                    lineNumber <- getLine
                    removeLineFromFile (inputFileName) (read (lineNumber) :: Int)
                "4" -> do
                    let outputFileName = if length args < 2 then inputFileName ++ " (copy)" else args !! 1
                    copyFileWithBlankLinesFilter inputFileName outputFileName
                "5" -> do
                    putStrLn "Введите строку, которую хотите добавить: "
                    keyword <- getLine
                    let outputFileName = if length args < 2 then inputFileName ++ " (copy)" else args !! 1
                    copyFileWithKeyWordFilter inputFileName outputFileName keyword
                _ -> putStrLn "Неверная операция"
