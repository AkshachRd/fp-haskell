import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO
import Data.List (delete)
import Control.Monad (when)

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ error "Specify the file name"
    let [fileName] = args

    inputFileExists <- doesFileExist fileName
    when (not inputFileExists) $ error $ "File " ++ fileName ++ " does not exist"

    putStrLn "Выберите операцию: 1 - Просмотр, 2 - Добавление, 3 - Удаление, 4 - Копирование с фильтрацией"
    operation <- getLine
    case operation of
        "1" -> do
            content <- readFile fileName
            putStrLn content
        "2" -> do
            putStrLn "Введите строку для добавления:"
            newLine <- getLine
            appendFile fileName (newLine ++ "\n")
        "3" -> do
            putStrLn "Введите строку для удаления:"
            lineNumberToDelete <- getLine
            let lineNumber = read lineNumberToDelete :: Int
            contents <- readFile fileName
            let lineList = lines contents
            let lineToDelete = lineList !! lineNumber
            let newContents = unlines $ filter (/= lineToDelete) lineList
            writeFile fileName newContents
        "4" -> do
            putStrLn "Введите имя нового файла для копирования:"
            newFileName <- getLine
            content <- readFile fileName
            let filteredContent = unlines . filter (not . null) . lines $ content
            when (length filteredContent > 0) $
                writeFile newFileName filteredContent
        _ -> putStrLn "Неверная операция"
