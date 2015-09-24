-- based on Functional Specification of Algorithms course in 2015 by Jan van Eijck

import Data.List
import System.Environment
import System.Random
import Picosat


type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]


showVal :: Value -> String
showVal 0 = " "
showVal d = show d



type Sudoku = (Row,Column) -> Value

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

emptyS :: Grid
emptyS = [[0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0]]

columns :: Sudoku -> [[Value]]
columns s = [[s (r, c) | r <- [1..9]] | c <- [1..9]] 

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

-- this is the actual sudoku construction!
grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 


showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
  do 
    putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

--main = print (mylength "haskell")

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 
get_num r c i = r * 100 + c * 10 + i
get_neg_num r c i = (-1) * (get_num r c i)

get_r n = quot n 100
get_c n = quot n 10 - ((get_r n) * 10)
get_i n = n `mod` 10

one_nine = [[get_num r c i | i <- positions]| r <-positions, c <- positions]

unique_cell = [[get_neg_num r c i , get_neg_num r c j]| r <-positions, c <- positions, i <- positions, j<-positions, not (i ==j)]

-- encode each column

encode_column = [[get_neg_num r i c, get_neg_num r j c]| r <-positions, c <- positions, i <- positions, j<-positions, not (i ==j)]

-- encode each row

encode_rows = [[get_neg_num i r  c, get_neg_num  j r c]| r <-positions, c <- positions, i <- positions, j<-positions, not (i ==j)]

-- each number

encode_num g = [ [get_num r c (((g !! (r-1) ) !! (c-1) ))]| r <-positions, c <- positions, not (((g !! (r-1)) !! (c-1) ) == 0)]

-- each subgrid

--encode_subgrid = [[] | r <-positions, c <- positions , i <- positions]
posi = [ (r,c) | r <- [1,4,7], c <- [1,4,7]]
posilist = [[ (r',c') | r' <- [r+0, r+1, r+2], c' <- [c+1, c+2, c+0]]  | (r,c) <- posi]
encode_subgrid = concat [[[get_neg_num r c i , get_neg_num r' c' i] | (r,c) <- p, (r',c') <- p , not((r,c) == (r',c')) ] |
                     i <- positions, p <- posilist]
--encode_subgrid = [[get_neg_num r' c' i |  (r,c) <- posi , r' <- [r+1, r+2, r+0], c' <- [c+1, c+2, c+0]] | i <- positions]


--take a grid and solve/verify it
solve_sudoku g =
  --let encode_subgrid = [] in 
  let f s x = if x < 0 then s else update s ((get_r x, get_c x), get_i x) in
  let init_s = (grid2sud emptyS) in  
  do 
    Solution solution <- solve (one_nine ++ unique_cell ++ encode_rows ++ encode_column ++ (encode_num g) ++ encode_subgrid)
    showSudoku (foldl f init_s solution)

main = do
    [f] <- getArgs
    s     <- readFile f
    let ls = lines s
    let wds = map words ls
    let r = \x -> fst ( head (reads x :: [(Int,String)]))
    let grid = map (\x -> map r x) wds
    showGrid grid
    putStrLn "Picosat solved your Sudoku problem and a solution is as follows"
    solve_sudoku grid
    --writeFile g s
