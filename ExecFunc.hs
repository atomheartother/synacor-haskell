module ExecFunc(
    ExecFuncType,
    exit,
    set,
    out,
    noop,
    jmp,
    jt,
    jf
) where

import System.IO
import qualified Data.Char as Char
import qualified VM as VM

-- Typedef for exec functions
type ExecFuncType = Handle -> VM.VMState -> [Int] -> IO VM.VMState

exit :: ExecFuncType
exit _ vm _ = return $ VM.exit vm

set :: ExecFuncType
set _ vm [] = error "set with no args"
set _ vm [x] = error "set with one arg instead of two"
set _ vm (x:y:xs) = return (VM.set vm a b)
    where
        a = x
        b = VM.get vm y

seek :: Handle -> Int -> IO ()
seek h address = hSeek h AbsoluteSeek $ seekIdx
    where seekIdx = 2 * (toInteger address)

jmp :: ExecFuncType
jmp _ _ [] = error "jmp with no args"
jmp h vm (x:xs) = do
    seek h address
    return vm
    where
        address = VM.get vm x

jt :: ExecFuncType
jt _ _ [] = error "jt with no args"
jt h vm (x:xs)
    | val /= 0 = jmp h vm xs
    | otherwise = return vm
    where val = VM.get vm x

jf :: ExecFuncType
jf _ _ [] = error "jf with no args"
jf h vm (x:xs)
    | val == 0 = jmp h vm xs
    | otherwise = return vm
    where val = VM.get vm x
    
    
out :: ExecFuncType
out _ _ [] = error "out with no args"
out _ vm (x:args) = do
    putChar $ Char.chr x
    return vm

noop :: ExecFuncType
noop _ vm _ = return vm
