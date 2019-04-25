-- FILTER
par :: Int -> Bool
par v = v `mod` 2 == 0

x5 :: Int -> Int
x5 v = v * 5

a = map x5(filter par [11,20,32,16,40,51])
main = print (show a)

-- "[100,160,80,200]"