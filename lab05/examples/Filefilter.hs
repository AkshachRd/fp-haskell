import System.IO
import Data.Char 

main = readFile "file.in"                       >>= \ s ->
       writeFile "output-file" (filter isAscii s)  >>
       putStr "Фильтрация завершилась успешно\n"