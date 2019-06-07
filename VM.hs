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
    seek,
    vmRead,
    write
) where

import Data.List
import qualified Data.Sequence as S
import qualified Data.Char as Char

type Registers = S.Seq Int
type Memory = S.Seq Char

maxIntSize = 32768
registerCount = 8

-- VM State contains a stack plus registers
data VMState = VMState {
    stack       :: [Int],       -- Stack is a list of ints
    registers   :: Registers,   -- Registers is a Sequence of ints
    memory      :: Memory,      -- Memory represents the program loaded in the VM
    close       :: Bool,        -- Boolean representing whether or not we need to close the VM
    ri          :: Int          -- Instruction pointer, value is the address, not the physical bytecount (which is address*2)
}

new :: [Char] -> VMState
new mem = VMState {stack = [], registers = S.replicate registerCount 0, memory = S.fromList mem, close = False, ri=0}

memSize :: VMState -> Int
memSize vm = S.length $ memory vm

-- Returns the physical position of ri in the buffer, in bytes
riPos :: VMState -> Int
riPos vm = (ri vm) * 2

-- Read memory at address. Doesn't affect the vm
vmRead :: VMState -> Int -> Int
vmRead vm address = Char.ord hi * 256 + Char.ord lo
    where
        lo = S.index (memory vm) $ address * 2
        hi = S.index (memory vm) $ address * 2 + 1

-- Read a value over 2 bytes and move ri to the next address
getVal :: VMState -> (Int, VMState)
getVal vm = (val, VMState{stack=stack vm, registers = registers vm, memory = memory vm, close = close vm, ri = (ri vm) + 1})
    where
        val = vmRead vm (ri vm)

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
    | reg < maxIntSize = error ("lval must be in register address range: " ++ show reg)
    | reg > maxIntSize + registerCount - 1 = error ("lval address out of bounds: " ++ show reg)
    | otherwise = reg - maxIntSize

rval :: VMState -> Int -> Int
rval vm n
    | n < maxIntSize = n -- Raw value
    | otherwise = getRegVal vm (lval n) -- Register value
    
-- Set a register value
set :: VMState -> Int -> Int -> VMState
set vm reg val
    | reg >= registerCount = error ("Trying to set invalid register: " ++ show reg)
    | True = VMState {stack = stack vm, registers = S.update reg (val `mod` maxIntSize) (registers vm), close = close vm, memory = memory vm , ri = ri vm }

pop :: VMState -> (Int, VMState)
pop VMState{stack=[]} = error "Trying to pop an empty stack"
pop VMState{stack=x:xs, registers=registers, close=close, ri=ri, memory=memory} = (x, VMState{stack=xs, registers = registers, close = close, memory = memory, ri = ri})

push :: VMState -> Int -> VMState
push vm val = VMState{stack = val:stack vm, registers = registers vm, close = close vm,  memory = memory vm, ri = ri vm}

exit :: VMState -> VMState
exit vm = VMState{stack = stack vm, registers = registers vm, close = True,  memory = memory vm, ri = ri vm}

seek :: VMState -> Int -> VMState
seek vm address 
    | (address * 2) > memSize vm = error ("Seeking out of bounds to " ++ show address)
    | otherwise = VMState{stack = stack vm, registers = registers vm, close = close vm,  memory = memory vm, ri = address}

-- Write to memory at address
write :: VMState -> Int -> Int -> VMState
write vm address val = VMState{stack = stack vm, registers = registers vm, close = close vm, memory = S.update (idx+1) hi $ S.update idx lo $ memory vm , ri = ri vm}
    where
        idx = address * 2
        lo = Char.chr $ val `mod` 256
        hi = Char.chr $ val `quot` 256