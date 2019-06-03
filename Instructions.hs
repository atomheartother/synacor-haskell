module Instructions(
    Instruction(..),
    nextInstruction
) where

import System.IO
import qualified Data.Map as Map  
import qualified Data.Char as Char

data Instruction = Instruction {
    opCode  :: Int,
    args    :: [Int]
} deriving (Show)

opCodeToArgCount :: Map.Map Int Int
opCodeToArgCount = Map.fromList [
    (0, 0),
    (1, 2),
    (2, 1),
    (3, 1),
    (4, 3),
    (5, 3),
    (6, 1),
    (7, 2),
    (8, 2),
    (9, 3),
    (10, 3),
    (11, 3),
    (12, 3),
    (13, 3),
    (14, 2),
    (15, 2),
    (16, 2),
    (17, 1),
    (18, 0),
    (19, 1),
    (20, 1),
    (21, 0)]

-- Read the next 2 bytes and puts them in an int
getArg :: Handle -> IO Int
getArg h = do
    lo <- hGetChar h
    done <- hIsEOF h
    hi <- if not done then hGetChar h else return '\0'
    return (Char.ord hi * 256 + Char.ord lo)

-- Takes an opCode and argCount and forms an Instruction object
makeInstruction :: Handle -> Int -> Maybe Int -> IO Instruction
makeInstruction _ _ Nothing = return (Instruction {opCode = -1, args = []})
makeInstruction h opCode (Just argCount) = do
    args <- mapM getArg [h | _ <- [1..argCount]]
    return (Instruction {opCode = opCode, args = args })

-- Takes a file handle and returns the next instruction
nextInstruction :: Handle -> IO Instruction
nextInstruction h = do
    opCode <- getArg h
    makeInstruction h opCode (Map.lookup opCode opCodeToArgCount)
