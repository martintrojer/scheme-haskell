module Env(envLookup, addEntry, addFrame, dropFrame) where

import Data.Maybe
import qualified Data.Map as M

import Types

envLookup :: String -> [Frame] -> Expr
envLookup s (f:rest) = fromMaybe (envLookup s rest) (M.lookup s f)
envLookup s _ = error $ "Unknown symbol '" ++ s ++ "'"

addEntry :: String -> Expr -> [Frame] -> [Frame]
addEntry s ex (f:rest) = M.insert s ex f : rest
addEntry _ _ _ = error "Inavlid environment"

addFrame :: Env -> Env
addFrame env = Env $ M.empty : getEnv env

dropFrame :: Env -> Env
dropFrame = Env . tail . getEnv
