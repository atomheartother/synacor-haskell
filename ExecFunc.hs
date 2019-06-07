module ExecFunc(
    ExecFuncType,
    exit,
    set,
    eq,
    gt,
    out,
    noop,
    jmp,
    jt,
    jf,
    add,
    push,
    pop,
    mult,
    myMod,
    myAnd,
    myOr,
    myNot,
    rmem,
    wmem,
    call,
    ret
) where

import qualified Data.Char as Char
import qualified VM as VM
import Data.Bits

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
pop vm (x:xs) = return (VM.set newVm reg val)
    where
        reg = VM.lval x
        (val, newVm) = VM.pop vm

compareVals :: (Int -> Int -> Bool) -> VM.VMState -> Int -> Int -> Int -> IO VM.VMState
compareVals f vm x y z
    | f b c = return (VM.set vm a 1)
    | otherwise = return (VM.set vm a 0)
    where
        a = VM.lval x
        b = VM.rval vm y
        c = VM.rval vm z

eq :: ExecFuncType
eq vm (x:y:z:xs) = compareVals (==) vm x y z

gt :: ExecFuncType
gt vm (x:y:z:xs) = compareVals (>) vm x y z

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

-- Stores into x the result of b `f` c
op :: (Int -> Int -> Int) -> VM.VMState -> Int -> Int -> Int -> IO VM.VMState
op f vm x y z = return (VM.set vm a $ b `f` c)
    where
        a = VM.lval x
        b = VM.rval vm y
        c = VM.rval vm z

add :: ExecFuncType
add vm (x:y:z:xs) = op (+) vm x y z

mult :: ExecFuncType
mult vm (x:y:z:xs) = op (*) vm x y z

myMod :: ExecFuncType
myMod vm (x:y:z:xs) = op mod vm x y z

myAnd :: ExecFuncType
myAnd vm (x:y:z:xs) = op (.&.) vm x y z

myOr :: ExecFuncType
myOr vm (x:y:z:xs) = op (.|.) vm x y z

myNot :: ExecFuncType
myNot vm (x:y:xs) = return (VM.set vm a $ complement b)
    where
        a = VM.lval x
        b = VM.rval vm y

rmem :: ExecFuncType
rmem vm (x:y:xs) = return (VM.set vm a memVal)
    where
        a = VM.lval x
        b = VM.rval vm y
        memVal = VM.vmRead vm b 

wmem :: ExecFuncType
wmem vm (x:y:xs) = return (VM.write vm a b)
    where
        a = VM.rval vm x
        b = VM.rval vm y
      
call :: ExecFuncType
call vm (x:xs) = return $ seek newVm address 
    where
        newVm = VM.push vm $ VM.ri vm
        address = VM.rval vm x

ret :: ExecFuncType
ret vm _ = return $ seek newVm address where (address, newVm) = VM.pop vm
        
out :: ExecFuncType
out _ [] = error "out with no args"
out vm (x:args) = do
    putChar $ Char.chr c
    return vm
    where c = VM.rval vm x

noop :: ExecFuncType
noop vm _ = return vm
