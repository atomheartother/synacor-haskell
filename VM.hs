module VM(
    VMState(..),
    get,
    getRegVal,
    set,
    new,
    exit
) where

import Data.List

-- 8 registers
type Registers = [Int]

-- VM State contains a stack plus registers
data VMState = VMState {
    stack       :: [Int],
    registers   :: Registers,
    close       :: Bool
}

new :: VMState
new = VMState {stack = [], registers = replicate 8 0, close=False}

-- Value has to be a valid register value
regCodeToIdx :: Int -> Int
regCodeToIdx reg
    | reg < 32768 = error ("Value is a regular number: " ++ show reg)
    | reg > 32775 = error ("Register value out of bounds: " ++ show reg)
    | otherwise = reg - 32768

-- Returns a register's value given a register index
getRegVal :: VMState -> Int -> Int
getRegVal vm idx = registers vm !! idx

get :: VMState -> Int -> Int
get vm n
    | n < 32768 = n -- Raw value
    | otherwise = getRegVal vm (regCodeToIdx n) -- Register value

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs
    
-- Set a register value
set :: VMState -> Int -> Int -> VMState
set vm n val= VMState {stack = stack vm, registers = replaceNth (regCodeToIdx n) val (registers vm), close = close vm }

pop :: VMState -> Int -> VMState
pop VMState{stack=[]} _ = error "Trying to pop an empty stack"
pop VMState{stack=x:xs, registers=registers, close=close} reg = VMState{stack=xs, registers= replaceNth (regCodeToIdx reg) x registers, close = close}

push :: VMState -> Int -> VMState
push vm val = VMState{stack = stack vm, registers = val:registers vm, close = close vm}

exit :: VMState -> VMState
exit vm = VMState{stack = stack vm, registers = registers vm, close = True}