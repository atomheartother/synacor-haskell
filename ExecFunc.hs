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
type ExecFuncType = Handle -> VM.VMState -> [Int] -> IO VM.VMState

exit :: ExecFuncType
exit _ vm _ = return $ VM.exit vm

set :: ExecFuncType
set _ vm [] = error "set with no args"
set _ vm [x] = error "set with one arg instead of two"
set _ vm (x:y:xs) = return (VM.set vm a b)
    where
        a = VM.lval x
        b = VM.rval vm y

push  :: ExecFuncType
push _ vm (x:xs) = return (VM.push vm a) where a = VM.rval vm x

pop  :: ExecFuncType
pop _ vm (x:xs) = return (VM.pop vm a) where a = VM.lval x

eq :: ExecFuncType
eq _ vm (x:y:z:xs)
        | b == c = return (VM.set vm a 1)
        | otherwise = return vm
        where
            a = VM.lval x
            b = VM.rval vm y
            c = VM.rval vm z

seek :: Handle -> Int -> IO ()
seek h address = hSeek h AbsoluteSeek $ seekIdx
    where seekIdx = 2 * (toInteger address)

jmp :: ExecFuncType
jmp _ _ [] = error "jmp with no args"
jmp h vm (x:xs) = do
    seek h address
    return vm
    where
        address = VM.rval vm x

jt :: ExecFuncType
jt _ _ [] = error "jt with no args"
jt h vm (x:xs)
    | val /= 0 = jmp h vm xs
    | otherwise = return vm
    where val = VM.rval vm x

jf :: ExecFuncType
jf _ _ [] = error "jf with no args"
jf h vm (x:xs)
    | val == 0 = jmp h vm xs
    | otherwise = return vm
    where val = VM.rval vm x
    
add :: ExecFuncType
add h vm (x:y:z:xs) = return (VM.set vm a (b+c))
    where
        a = VM.lval x
        b = VM.rval vm y
        c = VM.rval vm z

out :: ExecFuncType
out _ _ [] = error "out with no args"
out _ vm (x:args) = do
    putChar $ Char.chr x
    return vm

noop :: ExecFuncType
noop _ vm _ = return vm
