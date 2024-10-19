import System.Environment (getArgs)
import Data.Char (isPunctuation)
import Control.Monad (when)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ error "Specify the source and target file names"
    let [sourceFile, targetFile] = args
    putStrLn "Введите символ замены для знаков пунктуации:"
    replacement <- getLine
    content <- readFile sourceFile
    let newContent = map (\c -> if isPunctuation c then head replacement else c) content
    writeFile targetFile newContent
