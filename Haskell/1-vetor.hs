v = [1,2,3]
a = head v
b = tail v
c = 0 : v

main = putStrLn("\n a = " ++(show a) ++ "\n b = " ++ (show b) ++ "\n c = " ++ (show c))
{-
 a = 1
 b = [2,3]
 c = [0,1,2,3]
 -}