{-
Javascript Generator


Generates varied syntactically valid javascript according to the ECMA-262
specification: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-262.pdf

-}

module Main
  where
import JSGenerator (getAll, program)
import System.Directory
import System
import Text.Printf


mkcd dirname = do createDirectoryIfMissing False dirname
                  setCurrentDirectory dirname

filesPerDirectory :: Int
filesPerDirectory = 20000

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto k l = h:(splitInto k t)
        where (h,t) = splitAt k l


readArgs [s] = read s
readArgs _   = toEnum filesPerDirectory

main = do args <- getArgs
          case args of
            [arg]     -> generatePrograms (read arg)
            otherwise -> usage
generatePrograms n = do mkcd "gen"
                        mapM_ handleGroup $ zip [1..] $ splitInto filesPerDirectory $ zip [1..] $ take n $ getAll $ program

usage = do putStrLn "Usage: generatejs <number of js programs to generate>"
           putStrLn "  -- output is put into ./gen"

format :: Int -> String
format n = printf "%08d" n

handleGroup (n,ps) = do mkcd $ format n
                        mapM_ writeProgramToFile $ ps
                        setCurrentDirectory ".."

writeProgramToFile (n, p) = writeFile (format n ++ ".js") p




