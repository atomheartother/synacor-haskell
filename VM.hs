module VM(
    VMState(..),
    get,
    set,
    new
) where

import Data.List

-- 8 registers
type Registers = [Int]

-- VM State contains a stack plus registers
data VMState = VMState {
    stack       :: [Int],
    registers   :: Registers
}

new :: VMState
new = VMState {stack = [], registers = replicate 8 0}

get :: VMState -> Int -> Int
get vm n
    | n >= 32776 = error "rvalue out of bounds"
    | n < 32768 = n
    | otherwise = registers vm !! (n - 32768)


regCodeToIdx :: Int -> Int
regCodeToIdx reg = reg - 32768

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs
    
-- Set a register value
set :: VMState -> Int -> Int -> VMState
set vm n val
    | n >= 32776 = error "lvalue out of bounds"
    | n < 32768 = error "Can't use an integer as an lvalue"
    | otherwise = VMState {stack = stack vm, registers = replaceNth (regCodeToIdx n) val (registers vm) }

pop :: VMState -> Int -> VMState
pop VMState{stack=[]} _ = error "Trying to pop an empty stack"
pop VMState{stack=x:xs, registers=registers} reg = VMState{stack=xs, registers= replaceNth (regCodeToIdx reg) x registers }

push :: VMState -> Int -> VMState
push vm val = VMState{stack = stack vm, registers = val:registers vm}