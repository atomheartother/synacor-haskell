import System.IO
import Instructions
import VM

runVm:: VMState -> IO ()
runVm VMState{close=True} = return () -- Exit command was run
runVm vm = do
        newVm <- exec $ nextInstruction vm
        runVm newVm               -- Run the vm with the new state

-- runVm h vm = do
--     done <- hIsEOF h
--     if done then return () -- File is done
--     else do
--         i <- nextInstruction h vm   -- Grab the next instruction
--         putStrLn (show (op i) ++ ", args: " ++ show (args i))
--         newVm <- exec i
--         runVm h newVm               -- Run the vm with the new state

main:: IO ()
main = do
    h <- openBinaryFile "challenge.bin" ReadMode
    fileContents <- hGetContents h
    runVm $ VM.new fileContents
    hClose h
