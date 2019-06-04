module ExecFunc(
    ExecFuncType,
    exit,
    out,
    noop
) where

import System.IO
import qualified Data.Char as Char
import qualified VM as VM

-- Typedef for exec functions
type ExecFuncType = Handle -> VM.VMState -> [Int] -> IO VM.VMState

-- exit function
exit :: ExecFuncType
exit _ vm _ = return $ VM.exit vm

-- out function
out :: ExecFuncType
out _ vm args = do
    putChar $ Char.chr $ args !! 0
    return vm

-- noop function
noop :: ExecFuncType
noop _ vm _ = return vm
