module VM(
    VMState(..),
    rval,
    lval,
    getRegVal,
    set,
    new,
    exit,
    push,
    pop
) where

import Data.List
import Debug.Trace

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

-- Returns a register's value given a register index
getRegVal :: VMState -> Int -> Int
getRegVal vm idx = registers vm !! idx

-- Return a register index given a register code
lval :: Int -> Int
lval reg
    | reg < 32768 = error ("lval must be in register address range: " ++ show reg)
    | reg > 32775 = error ("lval address out of bounds: " ++ show reg)
    | otherwise = reg - 32768

rval :: VMState -> Int -> Int
rval vm n
    | n < 32768 = n -- Raw value
    | otherwise = getRegVal vm (lval n) -- Register value

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs
    
-- Set a register value
set :: VMState -> Int -> Int -> VMState
set vm reg val= VMState {stack = stack vm, registers = replaceNth reg (val `mod` 32768) (registers vm), close = close vm }

pop :: VMState -> Int -> VMState
pop VMState{stack=[]} _ = error "Trying to pop an empty stack"
pop VMState{stack=x:xs, registers=registers, close=close} reg = VMState{stack=xs, registers= replaceNth reg x registers, close = close}

push :: VMState -> Int -> VMState
push vm val = VMState{stack = val:stack vm, registers = registers vm, close = close vm}

exit :: VMState -> VMState
exit vm = VMState{stack = stack vm, registers = registers vm, close = True}