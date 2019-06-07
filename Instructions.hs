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
inst :: ExecFuncType -> Int -> Int -> (Int, Instruction)
inst f opCode argCount = (opCode,  Instruction opCode argCount (\vm -> \args -> Invocation {op=opCode, vm=vm, args=args, exec = f vm args}))

opCodeToInstruction :: Map.Map Int Instruction
opCodeToInstruction = Map.fromList [
        inst exit 0 0,
        inst set 1 2,
        inst push 2 1,
        inst pop 3 1,
        inst eq 4 3,
        inst gt 5 3,
        inst jmp 6 1,
        inst jt 7 2,
        inst jf 8 2,
        inst add 9 3,
        inst mult 10 3,
        inst myMod 11 3,
        inst myAnd 12 3,
        inst myOr 13 3,
        inst myNot 14 2,
        inst rmem 15 2,
        inst wmem 16 2,
        inst call 17 1,
        inst ret 18 0,
        inst out 19 1,
        inst readIn 20 1,
        inst noop 21 0
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