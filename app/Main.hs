module Main where

import Control.Monad.RWS         (runRWST, MonadState(put, get), liftIO)
import Data.Sequence             (Seq)
import Interstacker.Instructions ( Instruction, Value )
import Interstacker.ProgramState ( PState(..), PMonad )
import System.Environment        (getArgs)

import qualified Data.Sequence             as Seq
import qualified Interstacker.Instructions as Ins
import qualified Interstacker.ProgramState as PS

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            rawProgram <- readFile file
            let labeledProgram = map Ins.breakInstrLabel $ filter (not . null) $ lines rawProgram
                instrs = mapM fst labeledProgram
                labels = map snd labeledProgram

            case instrs of
                Nothing -> putStrLn "There is some syntax error in the input file."
                Just _insts -> do
                    _ <- runRWST (loop (Seq.fromList _insts)) () (PS.initState labels)
                    putStrLn "The program was executed successfully"
            return ()
        _ ->  putStrLn "Please specify the file to be interpreted"


loop :: Seq Instruction -> PMonad ()
loop insts = loop'
  where
    loop' = do
        st <- get
        let pc = programCount st
            mInst = Seq.lookup pc insts
        case mInst of
            Nothing -> return ()
            Just inst -> do
                put $ st { programCount = pc + 1 }
                PS.evalInstruction inst
                loop'
