import System.IO
import Instructions
import VM

runVm:: VMState -> IO ()
runVm VMState{close=True} = return () -- Exit command was run
runVm vm = do
        newVm <- exec $ nextInstruction vm
        runVm newVm               -- Run the vm with the new state

main:: IO ()
main = do
    h <- openBinaryFile "challenge.bin" ReadMode
    fileContents <- hGetContents h
    runVm $ VM.new fileContents
    hClose h
