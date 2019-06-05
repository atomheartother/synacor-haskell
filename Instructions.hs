module Instructions(
    Instruction(..),
    Invocation(..),
    nextInstruction
) where

import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified VM as VM
import ExecFunc

-- Represents an instruction.
-- Can provide an invocation of it if provided with a handle and a VMState
data Instruction = Instruction {
    opCode      :: Int,
    argCount    :: Int,
    with        :: VM.VMState -> [Int] -> Invocation
}

-- Represents a specific invocation of an instruction
-- For the same handle and vm state, will give the same result
data Invocation = Invocation {
    op      :: Int,         -- Opcode
    args    :: [Int],       -- Arguments
    vm      :: VM.VMState,  -- VM state before execution
    exec    :: IO VM.VMState   -- VM state after execution
}

-- Helper function to build the instruction dict
buildInstruction :: ExecFuncType -> Int -> Int -> (Int, Instruction)
buildInstruction f opCode argCount = (opCode,  Instruction opCode argCount (\vm -> \args -> Invocation {op=opCode, vm=vm, args=args, exec = f vm args}))

opCodeToInstruction :: Map.Map Int Instruction
opCodeToInstruction = Map.fromList [
        buildInstruction exit 0 0,
        buildInstruction set 1 2,
        buildInstruction push 2 1,
        buildInstruction pop 3 1,
        buildInstruction eq 4 3,
        buildInstruction jmp 6 1,
        buildInstruction jt 7 2,
        buildInstruction jf 8 2,
        buildInstruction add 9 3,
        buildInstruction out 19 1,
        buildInstruction noop 21 0
    ]

-- Takes an instruction and a vm state and returns an invocation object
makeInvocation :: VM.VMState -> Instruction -> Invocation
makeInvocation vm i = (with i) nextVm args where (args, nextVm) = VM.getVals vm (argCount i)

-- Takes a vm state and returns the next VM state
nextInstruction :: VM.VMState -> Invocation
nextInstruction vm = case (Map.lookup opCode opCodeToInstruction) of
        Nothing -> error ("Unknown opCode provided: " ++ show opCode)
        (Just instruction) -> makeInvocation nextVm instruction
        where
            (opCode, nextVm) = VM.getVal vm