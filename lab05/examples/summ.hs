import System.IO
main = do
         hSetBuffering stdout NoBuffering            
         putStr   "Введите новое целое число: "        
         x1 <- readNum 
         putStr   "Введите другое целое число: "          
         x2 <- readNum                          
         putStr  ("Их сумма равна " ++ show (x1+x2) ++ "\n")
       where readNum :: IO Integer
-- Указание сигнатуры типа позволяет избежать 
-- исправления типов x1,x2 правилом по умолчанию 
             readNum = readLn