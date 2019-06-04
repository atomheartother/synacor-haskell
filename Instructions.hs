module Instructions(
    Instruction(..),
    Invocation(..),
    nextInstruction
) where

import System.IO
import qualified Data.Map as Map  
import qualified Data.Char as Char
import qualified VM as VM

-- Represents an instruction.
-- Can provide an invocation of it if provided with a handle and a VMState
data Instruction = Instruction {
    opCode      :: Int,
    argCount    :: Int,
    with        :: Handle -> VM.VMState -> [Int] -> IO Invocation
}

-- Represents a specific invocation of an instruction
-- For the same handle and vm state, will give the same result
data Invocation = Invocation {
    op      :: Int,         -- Opcode
    h       :: Handle,      -- File handle
    vm      :: VM.VMState,  -- VM state before execution
    exec    :: VM.VMState   -- VM state after execution
}

opCodeToInstruction :: Map.Map Int Instruction
opCodeToInstruction = Map.fromList [
    (0,  Instruction 0 0 (\h -> \vm -> \args -> return Invocation {op=0, h=h, vm=vm, exec = VM.exit vm})),
    -- (1,  Instruction 1 2 Nothing),
    -- (2,  Instruction 2 1 Nothing),
    -- (3,  Instruction 3 1 Nothing),
    -- (4,  Instruction 4 3 Nothing),
    -- (5,  Instruction 5 3 Nothing),
    -- (6,  Instruction 6 1 Nothing),
    -- (7,  Instruction 7 2 Nothing),
    -- (8,  Instruction 8 2 Nothing),
    -- (9,  Instruction 9 3 Nothing),
    -- (10, Instruction 10 3 Nothing),
    -- (11, Instruction 11 3 Nothing),
    -- (12, Instruction 12 3 Nothing),
    -- (13, Instruction 13 3 Nothing),
    -- (14, Instruction 14 2 Nothing),
    -- (15, Instruction 15 2 Nothing),
    -- (16, Instruction 16 2 Nothing),
    -- (17, Instruction 17 1 Nothing),
    -- (18, Instruction 18 0 Nothing),
    (19, Instruction 19 1 (\h -> \vm -> \args -> do
        putChar $ Char.chr $ args !! 0
        return Invocation {op=21, h=h, vm=vm, exec = vm})),
    -- (20, Instruction 20 1 Nothing),
    (21, Instruction 21 0 (\h -> \vm -> \args -> return Invocation {op=21, h=h, vm=vm, exec = vm}))] -- Noop

-- Read the next 2 bytes and put them in an int
getArg :: Handle -> IO Int
getArg h = do
    lo <- hGetChar h
    done <- hIsEOF h
    hi <- if not done then hGetChar h else return '\0'
    return (Char.ord hi * 256 + Char.ord lo)

-- Takes an instruction and a vm state and returns an invocation object
makeInvocation :: Handle -> VM.VMState -> Instruction -> IO Invocation
makeInvocation h vm i = do
    args <- mapM getArg [h | _ <- [1..(argCount i)]]
    (with i) h vm args

-- Takes a file handle and returns the next instruction with proper args bound
nextInstruction :: Handle -> VM.VMState -> IO Invocation
nextInstruction h vm = do
    opCode <- getArg h
    case (Map.lookup opCode opCodeToInstruction) of
        Nothing -> error ("Unknown opCode provided: " ++ show opCode)
        (Just instruction) -> makeInvocation h vm instruction
