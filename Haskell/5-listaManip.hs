data Lista = Vazia | Par Int Lista
  deriving Show

m = (Par 10 (Par 20 (Par 30 (Par 40 Vazia))))

index'' :: Int -> Lista -> Int
index'' _  Vazia      = error "A lista esta vazia"
index'' 0  (Par h _)  = h
index'' idx (Par _ t) = index'' (idx - 1) t

main = print $ index'' 1 m