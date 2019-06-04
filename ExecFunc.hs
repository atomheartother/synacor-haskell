module ExecFunc(
    ExecFuncType,
    exit,
    out,
    noop,
    jmp
) where

import System.IO
import qualified Data.Char as Char
import qualified VM as VM

-- Typedef for exec functions
type ExecFuncType = Handle -> VM.VMState -> [Int] -> IO VM.VMState

exit :: ExecFuncType
exit _ vm _ = return $ VM.exit vm

seek :: Handle -> Int -> IO ()
seek h address = hSeek h AbsoluteSeek $ seekIdx
    where seekIdx = 2 * (toInteger address)

jmp :: ExecFuncType
jmp h vm args = do
    putStrLn $ "[VM] jmp " ++ show address
    seek h address
    return vm
    where
        address = VM.get vm $ args !! 0

out :: ExecFuncType
out _ vm args = do
    putChar $ Char.chr $ args !! 0
    return vm

noop :: ExecFuncType
noop _ vm _ = return vm
