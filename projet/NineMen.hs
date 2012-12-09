module NineMen where



{-

Tableau de 24 cases 
win si 3 jetons alignés

1           2          3

     4      5     6
        
        7   8   9
        
10   11 12     13 14  15

        16 17  18

    19     20    21

22         23         24

Lignes 

123
456
789
101112
131415
161718
192021
222324

Colones
1 10 22
4 11 19
7 12 16
2 5  8
17 20 23
9 13 18
6 14 21
3 15 24

Diagonales
i + [0|1|2] * 3 (avec i = [1, 3, 16, 18])
147
369
161922
182124


-}

data Board = Board [Case]
    deriving Show

data Token = Red | Black
    deriving Show

data Case = Token | Empty
    deriving Show

newBoard :: Board 
newBoard = Board [Empty | x <- [1..24]]

