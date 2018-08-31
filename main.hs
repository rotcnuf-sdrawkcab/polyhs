import EquationSolver
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

interactiveMain :: IO ()
interactiveMain = do
  putStr "Enter the equation $> "
  hFlush stdout
  l <- getLine
  case l of
    "" -> return ()
    _  -> do
      parseAndSolve l
      interactiveMain

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interactiveMain
    xs -> mapM_ parseAndSolve xs
