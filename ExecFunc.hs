module ExecFunc(
    ExecFuncType,
    exit,
    set,
    eq,
    out,
    noop,
    jmp,
    jt,
    jf,
    add,
    push,
    pop
) where

import System.IO
import Debug.Trace
import qualified Data.Char as Char
import qualified VM as VM

-- Typedef for exec functions
type ExecFuncType = VM.VMState -> [Int] -> IO VM.VMState

exit :: ExecFuncType
exit vm _ = return $ VM.exit vm

set :: ExecFuncType
set vm [] = error "set with no args"
set vm [x] = error "set with one arg instead of two"
set vm (x:y:xs) = return (VM.set vm a b)
    where
        a = VM.lval x
        b = VM.rval vm y

push  :: ExecFuncType
push vm (x:xs) = return (VM.push vm a) where a = VM.rval vm x

pop  :: ExecFuncType
pop vm (x:xs) = return (VM.pop vm a) where a = VM.lval x

eq :: ExecFuncType
eq vm (x:y:z:xs)
        | b == c = return (VM.set vm a 1)
        | otherwise = return vm
        where
            a = VM.lval x
            b = VM.rval vm y
            c = VM.rval vm z

seek :: VM.VMState -> Int -> VM.VMState
seek vm address = VM.seek vm address

jmp :: ExecFuncType
jmp _ [] = error "jmp with no args"
jmp vm (x:xs) = return (seek vm address)
    where
        address = VM.rval vm x

jt :: ExecFuncType
jt _ [] = error "jt with no args"
jt vm (x:xs)
    | val /= 0 = jmp vm xs
    | otherwise = return vm
    where val = VM.rval vm x

jf :: ExecFuncType
jf _ [] = error "jf with no args"
jf vm (x:xs)
    | val == 0 = jmp vm xs
    | otherwise = return vm
    where val = VM.rval vm x
    
add :: ExecFuncType
add vm (x:y:z:xs) = return (VM.set vm a (b+c))
    where
        a = VM.lval x
        b = VM.rval vm y
        c = VM.rval vm z

out :: ExecFuncType
out _ [] = error "out with no args"
out vm (x:args) = do
    putChar $ Char.chr x
    return vm

noop :: ExecFuncType
noop vm _ = return vm
