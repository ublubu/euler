module H1.H1
(main) where

last' :: [a] -> a
last' [] = error "no last element for empty"
last' (x: []) = x
last' (x:xs) = last' xs

lastbutone :: [a] -> a
lastbutone [] = error "no next-to-last for empty"
lastbutone [x] = error "no next-to-last for singleton"
lastbutone (x:[y]) = x
lastbutone (x:xs) = lastbutone xs

nth :: Int -> [a] -> a
nth _ [] = error "index out of bounds for list"
nth 0 (x:xs) = x
nth n (x:xs) = nth (n - 1) xs

len' :: [a] -> Int
len' [] = 0
len' (x:[]) = 1
len' (x:xs) = 1 + len' xs

rev :: [a] -> [a]
rev [] = []
rev (x:[]) = [x]
rev (x:xs) = rev xs ++ [x]

isPal :: Eq a => [a] -> Bool
isPal xs = xs == (rev xs)

main :: IO ()
main = do
  print $ last' "asdf"
  print $ lastbutone "asdf"
  print $ nth 1 "asdf"
  print $ len' "asdf"
  print $ rev "asdf"
  print $ isPal "asdfdsa"
