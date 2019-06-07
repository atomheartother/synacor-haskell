import System.IO
import Instructions
import VM
import Debug.Trace

runVm:: VMState -> IO ()
runVm VMState{close=True} = return () -- Exit command was run
runVm vm = do
        newVm <- exec i
        runVm newVm               -- Run the vm with the new state
        where i = nextInstruction vm

main:: IO ()
main = do
    h <- openBinaryFile "challenge.bin" ReadMode
    fileContents <- hGetContents h
    runVm $ VM.new fileContents
    hClose h
