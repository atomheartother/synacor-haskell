import System.IO
import Instructions
import VM

printInstructions:: Handle -> IO ()
printInstructions h = do
    done <- hIsEOF h
    if done then return ()
    else do
        i <- nextInstruction h
        print $ show i
        printInstructions h

main:: IO ()
main = do
    h <- openFile "challenge.bin" ReadMode
    printInstructions h
    hClose h