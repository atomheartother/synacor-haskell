import System.IO
import Instructions
import VM

runVm:: Handle -> VMState -> IO ()
runVm _ VMState{close=True} = return ()
runVm h vm = do
    done <- hIsEOF h
    if done then return () -- File is done
    else do
        i <- nextInstruction h vm   -- Grab the next instruction
        runVm h $ exec i            -- Run the vm with the new state

main:: IO ()
main = do
    h <- openFile "challenge.bin" ReadMode
    runVm h VM.new
    hClose h