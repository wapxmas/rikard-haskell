{-# LANGUAGE TypeFamilies #-}

module Main where

data family T a
data instance T Int  = A
data instance T Char = B

nonsense :: a ~ Int => T a -> Int
nonsense A = 1

main :: IO ()
main = undefined
