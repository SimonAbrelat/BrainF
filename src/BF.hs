module BF where

import           Data.Char

data Mem a =
  Mem [a] a [a]
  deriving (Show)

data BFAST
  = Next
  | Prev
  | Inc
  | Dec
  | Out
  | In
  | Loop [BFAST]
  deriving (Show)

-- Variables
empty :: Mem Int
empty = Mem [] 0 []

-- Movement
next :: Mem Int -> Mem Int
next (Mem l v (r:rs)) = Mem (l ++ [v]) r rs
next (Mem l v [])     = Mem (l ++ [v]) 0 []

prev :: Mem Int -> Mem Int
prev (Mem (l:ls) v r) = Mem ls l (v : r)
prev (Mem [] v r)     = Mem [] 0 (v : r)

-- Modifying data
mid :: (a -> a) -> Mem a -> Mem a
mid f (Mem l v r) = Mem l (f v) r

inc :: Mem Int -> Mem Int
inc = mid (+ 1)

dec :: Mem Int -> Mem Int
dec = mid (subtract 1)

-- Eval
eval :: Mem Int -> [BFAST] -> IO (Mem Int)
eval m [] = return m
eval mem@(Mem l v r) loop@(x:xs) =
  case x of
    Next -> eval (next mem) xs
    Prev -> eval (prev mem) xs
    Inc -> eval (inc mem) xs
    Dec -> eval (dec mem) xs
    In -> do
      p <- getChar
      eval (Mem l (ord p) r) xs
    Out -> do
      putStr [(chr v)]
      eval (Mem l v r) xs
    Loop s ->
      if v /= 0
        then do
          mem' <- eval mem s
          eval mem' loop
        else eval mem xs

run :: [BFAST] -> IO (Mem Int)
run = eval empty
