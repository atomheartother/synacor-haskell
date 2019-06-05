module VM(
    VMState(..),
    rval,
    lval,
    getRegVal,
    set,
    new,
    exit,
    push,
    pop,
    getVal,
    getVals,
    memSize,
    seek
) where

import Data.List
import qualified Data.Sequence as S
import qualified Data.Char as Char
import Debug.Trace

-- 8 registers
type Registers = S.Seq Int
type Memory = S.Seq Char

-- VM State contains a stack plus registers
data VMState = VMState {
    stack       :: [Int],       -- Stack is a list of ints
    registers   :: Registers,   -- Registers is a Sequence of ints
    memory      :: Memory,      -- Memory represents the program loaded in the VM
    close       :: Bool,        -- Boolean representing whether or not we need to close the VM
    ri          :: Int          -- Instruction pointer, value is the bytecount we're at in memory, not the address (which is bytecount/2)
}

new :: [Char] -> VMState
new mem = VMState {stack = [], registers = S.replicate 8 0, memory = S.fromList mem, close = False, ri=0}

memSize :: VMState -> Int
memSize vm = S.length $ memory vm

getVal :: VMState -> (Int, VMState)
getVal vm = (Char.ord hi * 256 + Char.ord lo, VMState{stack=stack vm, registers = registers vm, memory = memory vm, close = close vm, ri = (ri vm) + 2})
    where
        lo = S.index (memory vm) $ ri vm
        hi = S.index (memory vm) $ (ri vm) + 1

getVals :: VMState -> Int -> ([Int], VMState)
getVals vm 0 = ([], vm)
getVals vm count = (thisArg:otherArgs, finalState) where
    (thisArg, nextState) = getVal vm
    (otherArgs, finalState) = getVals nextState (count - 1)

-- Returns a register's value given a register index
getRegVal :: VMState -> Int -> Int
getRegVal vm idx = S.index (registers vm) idx

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
    
-- Set a register value
set :: VMState -> Int -> Int -> VMState
set vm reg val= VMState {stack = stack vm, registers = S.update reg (val `mod` 32768) (registers vm), close = close vm, memory = memory vm , ri = ri vm }

pop :: VMState -> Int -> VMState
pop VMState{stack=[]} _ = error "Trying to pop an empty stack"
pop VMState{stack=x:xs, registers=registers, close=close, ri=ri, memory=memory} reg = VMState{stack=xs, registers= S.update reg x registers, close = close, memory = memory, ri = ri}

push :: VMState -> Int -> VMState
push vm val = VMState{stack = val:stack vm, registers = registers vm, close = close vm,  memory = memory vm, ri = ri vm}

exit :: VMState -> VMState
exit vm = VMState{stack = stack vm, registers = registers vm, close = True,  memory = memory vm, ri = ri vm}

seek :: VMState -> Int -> VMState
seek vm address = VMState{stack = stack vm, registers = registers vm, close = close vm,  memory = memory vm, ri = newRi}
    where
        newRi = address * 2