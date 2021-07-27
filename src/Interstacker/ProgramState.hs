module Interstacker.ProgramState () where

import Interstacker.Instructions
    ( Id
    , Label
    , Instruction(..)
    , Value(..)
    )

import Control.Monad.RWS
    ( MonadState(put, get)
    , MonadWriter(tell)
    , RWST
    , liftIO
    , when
    , unless
    )

import Data.Map             (Map)
import System.Exit          (exitSuccess)

import qualified Data.Map                  as Map
import qualified Interstacker.Instructions as Ins

data Element
    = Value Value
    | Address Id
    deriving (Eq, Show)

data PState = PState
    { stack        :: [Element]
    , addressSpace :: Map Id Element
    , labelMapping :: Map Label Int
    , programCount :: Int
    }

type PMonad = RWST () [String] PState IO

evalInstruction :: Instruction ->  PMonad ()
evalInstruction (Push val) = do
    st <- get
    let newStack = Value val : stack st
    put $ st { stack = newStack }
evalInstruction Pop = do
    st <- get
    case stack st of
        [] -> liftIO $ putStrLn "POP operation on empty stack!"
        stk -> put $ st { stack = tail stk }
evalInstruction Add = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (IntVal (y + x)) : rest}
        _ -> liftIO $ putStrLn "ADD operation on invalid stack!"
evalInstruction Sub = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (IntVal (y - x)) : rest}
        _ -> liftIO $ putStrLn "SUB operation on invalid stack!"
evalInstruction Mul = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (IntVal (y * x)) : rest}
        _ -> liftIO $ putStrLn "MUL operation on invalid stack!"
evalInstruction Div = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (IntVal (y `div` x)) : rest}
        _ -> liftIO $ putStrLn "DIV operation on invalid stack!"
evalInstruction Mod = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (IntVal (y `mod` x)) : rest}
        _ -> liftIO $ putStrLn "MOD operation on invalid stack!"
evalInstruction And = do
    st <- get
    case stack st of
        ((Value (BoolVal x)):(Value (BoolVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y && x)) : rest}
        _ -> liftIO $ putStrLn "AND operation on invalid stack!"
evalInstruction Or = do
    st <- get
    case stack st of
        ((Value (BoolVal x)):(Value (BoolVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y || x)) : rest}
        _ -> liftIO $ putStrLn "OR operation on invalid stack!"
evalInstruction Lt = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y < x)) : rest}
        ((Value (BoolVal x)):(Value (BoolVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y < x)) : rest}
        _ -> liftIO $ putStrLn "LT operation on invalid stack!"
evalInstruction Gt = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y > x)) : rest}
        ((Value (BoolVal x)):(Value (BoolVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y > x)) : rest}
        _ -> liftIO $ putStrLn "GT operation on invalid stack!"
evalInstruction Leq = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y <= x)) : rest}
        ((Value (BoolVal x)):(Value (BoolVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y <= x)) : rest}
        _ -> liftIO $ putStrLn "LEQ operation on invalid stack!"
evalInstruction Geq = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y >= x)) : rest}
        ((Value (BoolVal x)):(Value (BoolVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y >= x)) : rest}
        _ -> liftIO $ putStrLn "GEQ operation on invalid stack!"
evalInstruction Eq = do
    st <- get
    case stack st of
        ((Value (IntVal x)):(Value (IntVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y == x)) : rest}
        ((Value (BoolVal x)):(Value (BoolVal y)):rest) ->
            put $ st { stack = Value (BoolVal (y == x)) : rest}
        _ -> liftIO $ putStrLn "EQ operation on invalid stack!"
evalInstruction Neq = do
    st <- get
    case stack st of
        ( Value (IntVal x) : Value (IntVal y) : rest) ->
            put $ st { stack = Value (BoolVal (y /= x)) : rest}
        ( Value (BoolVal x) : Value (BoolVal y) : rest) ->
            put $ st { stack = Value (BoolVal (y /= x)) : rest}
        _ -> liftIO $ putStrLn "NEQ operation on invalid stack!"
evalInstruction Uminus = do
    st <- get
    case stack st of
        ( Value (IntVal x) : rest) ->
            put $ st { stack = Value (IntVal (-x)) : rest }
        _ -> liftIO $ putStrLn "UMINUS operation on invalid stack!"
evalInstruction Not = do
    st <- get
    case stack st of
        ( Value (BoolVal x) : rest) ->
            put $ st { stack = Value (BoolVal (not x)) : rest }
        _ -> liftIO $ putStrLn "NOT operation on invalid stack!"
evalInstruction Assign = do
    st <- get
    case stack st of
        (Address id : val@(Value _) : rest) -> do
            let newMap = Map.insert id val (addressSpace st)
            put $ st { addressSpace = newMap }
        _ -> liftIO $ putStrLn "ASSIGN operation on invalid stack!"
evalInstruction (Rvalue id) = do
    st <- get
    case Map.lookup id (addressSpace st) of
        Just val -> put $ st { stack = val : stack st }
        _ -> liftIO $ putStrLn "RVALUE operation on unassigned address!"
evalInstruction (Lvalue id) = do
    st <- get
    put $ st { stack = Address id : stack st }
evalInstruction (Goto label) = do
    st <- get
    case Map.lookup label (labelMapping st) of
        Just idx -> put $ st { programCount = idx }
        _ -> liftIO $ putStrLn "GOTO operation on non-existing label!"
evalInstruction (GoTrue label) = do
    st <- get
    case Map.lookup label (labelMapping st) of
        Just idx -> case stack st of
            ( Value (BoolVal val) : _ ) -> when val $ put $ st { programCount = idx }
            _ -> liftIO $ putStrLn "GOTRUE operation on invalid stack!"
        _ -> liftIO $ putStrLn "GOTRUE operation on non-existing label!"
evalInstruction (GoFalse label) = do
    st <- get
    case Map.lookup label (labelMapping st) of
        Just idx -> case stack st of
            ( Value (BoolVal val) : _ ) -> unless val $ put $ st { programCount = idx }
            _ -> liftIO $ putStrLn "GOFALSE operation on invalid stack!"
        _ -> liftIO $ putStrLn "GOFALSE operation on non-existing label!"
evalInstruction (Read id) = do
    st <- get
    liftIO $ putStr "$ > "
    input <- liftIO getLine
    case Ins.valueFromString input of
        Just val -> do
            let newMap = Map.insert id (Value val) (addressSpace st)
            put $ st { addressSpace = newMap }
        _ -> liftIO $ putStrLn "Invalid input value!"
evalInstruction (Print id) = do
    st <- get
    case Map.lookup id (addressSpace st) of
        Just val -> liftIO $ print val
        _ -> liftIO $ putStrLn "PRINT operation on unassigned address!"
evalInstruction Exit = liftIO exitSuccess

