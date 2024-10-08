import Prelude hiding (splitAt, elem)
import Data.Char (ord)
import qualified Data.Map as Map
import qualified Data.Set as Set

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs
  | n <= 0    = ([], xs)
  | n >= len  = (xs, [])
  | otherwise = (take n xs, drop n xs)
  where len = length xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs)
  | y == x    = True
  | otherwise = elem y xs

isAscii :: Char -> Bool
isAscii c = ord c >= 0 && ord c <= 127


member :: Ord k => k -> Map.Map k a -> Bool
member key map = case Map.lookup key map of
    Just _  -> True
    Nothing -> False

myMember :: Ord a => a -> Set.Set a -> Bool
myMember x set = found
    where (_, found, _) = Set.splitMember x set

main = do
  putStrLn "===slitAt==="
  print $ splitAt 3 "heyman"
  print $ splitAt 100 "heyman"
  print $ splitAt (-3) "heyman"
  print $ let (a,b) = splitAt 3 "foobar" in b ++ a
  putStrLn ""

  putStrLn "===elem==="
  print $ elem 3 [1, 2, 3, 4, 5]
  print $ elem 6 [1, 2, 3, 4, 5]
  print $ 'a' `elem` "hello"
  print $ 'o' `elem` "hello"
  putStrLn ""

  putStrLn "===isAscii==="
  print $ isAscii 'A'
  print $ isAscii 'z'
  print $ isAscii '€'
  print $ isAscii '\n'
  print $ isAscii 'ñ'
  putStrLn ""

  putStrLn "===member==="
  let map1 = Map.fromList [(1, "a"), (2, "b"), (3, "c")]
  print $ member 2 map1
  print $ member 4 map1
  print $ member 1 map1
  putStrLn ""

  putStrLn "===myMember==="
  let set1 = Set.fromList [1, 2, 3, 4, 5]
  print $ myMember 3 set1
  print $ myMember 6 set1
  print $ myMember 1 set1
  print $ myMember 0 set1
  putStrLn ""
