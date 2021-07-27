module Interstacker.Instructions
    ( Id
    , Label
    , Value(..)
    , Instruction(..)
    , breakInstrLabel
    , valueFromString
    , instrFromString
    ) where

import Data.Bifunctor   (bimap)
import Data.Tuple       (swap)
import Text.Read        (readMaybe)


type Id = String

type Label = String

data Value
    = BoolVal Bool
    | IntVal Int
    deriving (Eq, Show)

data Instruction
    = Push Value
    | Pop
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Lt
    | Gt
    | Leq
    | Geq
    | Eq
    | Neq
    | Uminus
    | Not
    | Assign
    | Rvalue Id
    | Lvalue Id
    | Goto Label
    | GoTrue Label
    | GoFalse Label
    | Read Id
    | Print Id
    | Exit
    deriving (Eq, Show)


valueFromString :: String -> Maybe Value
valueFromString "true" = Just $ BoolVal True
valueFromString "false" = Just $ BoolVal False
valueFromString str = IntVal <$> readMaybe str

instrFromString :: String -> Maybe Instruction
instrFromString str = case words str of
    ["POP"]          -> Just Pop
    ["ADD"]          -> Just Add
    ["Sub"]          -> Just Sub
    ["MUL"]          -> Just Mul
    ["DIV"]          -> Just Div
    ["MOD"]          -> Just Mod
    ["AND"]          -> Just And
    ["OR"]           -> Just Or
    ["LT"]           -> Just Lt
    ["GT"]           -> Just Gt
    ["LEQ"]          -> Just Leq
    ["GEQ"]          -> Just Geq
    ["EQ"]           -> Just Eq
    ["NEQ"]          -> Just Neq
    ["UMINUS"]       -> Just Uminus
    ["NOT"]          -> Just Not
    ["ASSIGN"]       -> Just Assign
    ["EXIT"]         -> Just Exit
    ["PUSH"   , val] -> Push <$> valueFromString val
    ["RVALUE" , id]  -> Just $ Rvalue id
    ["LVALUE" , id]  -> Just $ Lvalue id
    ["READ"   , id]  -> Just $ Read id
    ["PRINT"  , id]  -> Just $ Print id
    ["GOTO"   , id]  -> Just $ Goto id
    ["GOTRUE" , id]  -> Just $ GoTrue id
    ["GOFALSE", id]  -> Just $ GoFalse id
    _                -> Nothing

breakInstrLabel :: String -> (Maybe Instruction, Label)
breakInstrLabel = bimap instrFromString removeColon . break'
  where
    removeColon "" = ""
    removeColon label = init label
    break' = bimap reverse reverse . break (== ':') . reverse
