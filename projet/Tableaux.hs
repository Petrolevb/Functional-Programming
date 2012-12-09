module Tableau where


data Pair = Pair { x:: Int, y::Int }
    deriving Show

data Tableau = Tableau { rows::[[a]] }
    deriving Show

-- Check si la position dans le tableau est correcte
(!??) :: Tableau -> Pair -> Bool
(!??) t p | length (rows t)            >= (x p) = False 
          | length ((rows t) !! (x p)) >= (y p) = False
          | otherwise                           = True

-- Remplace la valeur dans une liste
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) t (i, v) | i < 0 || i >= length t = t
(!!=) t (i, v) = take i t ++ [v] ++ drop (i+1) t

-- Retourne l'élément (x, y) du tableau
(!!!) :: Tableau -> Pair -> a
(!!!) t p | t !?? p   = ((rows t) !! (x p)) !! (y p)
          | otherwise = error "Invalid position argument"

-- Change la case (x, y) du tableau par a
updateCase :: Tableau -> Pair -> a -> Tableau
updateCase t p i | t !?? p   = Tableau (take (x p) (rows t) ++ newLine ++ drop ((x p) + 1) (rows t))
                 | otherwise = t
        where newLine = [rows t !! (x p) !!= ((y p), i)]
