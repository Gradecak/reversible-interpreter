--I want these language extensions for my syntactic sugaring tricks at the end

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

--I want my own definition of lookup and I want to write my own function
--named "print".
module Interpreter where

import qualified Data.Map               as Map
import           Data.Maybe
import           Prelude                hiding (lookup, print)
-- I want to get at the standard "print" function using the name System.print
import qualified System.IO              as System
-- I plan to use these monads to construct the parts of my interpreter
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import Control.Monad (void, when)
import           Control.Monad.Writer
import System.Exit (exitSuccess)

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

data Val = I Int | B Bool
         deriving (Eq, Show, Read)

data Expr = Const Val
          | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
          | And Expr Expr | Or Expr Expr  | Not Expr
          | Eq Expr Expr  | Gt Expr Expr  | Lt Expr Expr
          | Var String
          deriving (Eq, Show, Read)

type Name = String
type Env  = Map.Map Name Val

lookup k t = case Map.lookup k t of
               Just x  -> return x
               Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator,
 -- with error handling and Reader monad instance to carry dictionary --}
type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

--This evaluator could be a little neater
--Integer typed expressions
evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

--Boolean typed expressions
evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

--Operations over integers which produce Boleans
evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

--Evaluate an expression
eval :: Expr -> Eval Val
eval (Const v   ) = return v
eval (Add e0 e1 ) = evali (+) e0 e1
eval (Sub e0 e1 ) = evali (-) e0 e1
eval (Mul e0 e1 ) = evali (*) e0 e1
eval (Div e0 e1 ) = evali div e0 e1
eval (And e0 e1 ) = evalb (&&) e0 e1
eval (Or e0 e1  ) = evalb (||) e0 e1
eval (Not e0    ) = evalb (const not) e0 (Const (B True))
                       where not2 a _ = not a -- hack,
eval (Eq e0 e1  ) = evalib (==) e0 e1
eval (Gt e0 e1  ) = evalib (>) e0 e1
eval (Lt e0 e1  ) = evalib (<) e0 e1
eval (Var s     ) = do env <- ask
                       lookup s env

{-------------------------------------------------------------------}
{- The statement language                                          -}
{-------------------------------------------------------------------}
data Statement = Assign Name Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass
               deriving (Eq, Show, Read)

type Run a = StateT [(Env, Statement)] (ReaderT [Statement] (ExceptT String IO)) a
runRun p =  runExceptT (runReaderT (runStateT p [(Map.empty, Pass)]) [])

-- setting a variable DOESN'T push a new state to the list.
-- Instead, it inserts the values into the enviroment found at the head of the list
set :: (Name, Val) -> Run ()
set (s,i) = state $ \table -> ((), (Map.insert s i (fst $ head table), snd $ head table):tail table)

-- push the current sate of the program (Current enviroment and last executerd statement)
-- to our list of states
saveCurrentState :: Statement -> Run ()
saveCurrentState s = state $ \table -> ((), (fst $ head table, s):table)

exec :: Statement -> Run ()
exec s@(Assign name v) = do
    prompt s
    ((env,_):_) <- get
    Right val <- return $ runEval env (eval v)
    set (name, val)

exec s@(Print e) = do
    prompt s
    ((env,_):_) <- get
    Right val <- return $ runEval env (eval e)
    liftIO $ System.print val

exec p@(While cond s) = do
    ((env,_):_) <- get
    Right (B val) <- return $ runEval env (eval cond)
    when val $ exec s >> prompt p >> exec (While cond s)

exec (Seq s0 s1) = exec s0 >> exec s1

{-Interpreter Utility Functions-}

-- pretty print function for displaying the value of a variable in the enviroment
pPrintVar :: Name -> Val -> IO ()
pPrintVar n = putStrLn . ((n ++ " := ") ++) . show

head' :: [a] -> Maybe a
head' [] = Nothing
head' x  = Just $ head x

-- rollback the state and execute previous statement
stepback :: Statement -> Run ()
stepback s = do
    old@((env, statement):xs) <- get -- get current state of the program
    put xs                           -- rollback state by 1 entry
    exec statement                   -- exec rolled back statement
    put old                          -- when done restore previous state
    prompt s

-- inspect the state of a variable in the current enviroment
inspectCurrent :: Name -> Run ()
inspectCurrent name = do
    ((env,_):_) <- get
    let x = Map.lookup name env
    liftIO $ case x of
      (Just val) -> pPrintVar name val
      Nothing    -> putStrLn "error"

-- inspect the state of a variable across all of the enviroments
inspectAll :: Name -> Run ()
inspectAll name = do
    states <- get
    let envs = map fst states
        x    = mapMaybe (Map.lookup name) envs
    liftIO $ mapM_ (pPrintVar name) x

prompt :: Statement -> Run ()
prompt s = do
    liftIO $ putStrLn $ show s ++ "?"
    prompt
  where prompt = do
            liftIO $ putStr ">"
            x <- liftIO $ words <$> getLine
            case head' x of
              Nothing        -> void $ saveCurrentState s
              Just "break"   -> void $ liftIO exitSuccess
              Just "inspect" -> inspectAll (last x) >> prompt
              Just "back"    -> stepback s
              _              -> liftIO (putStrLn "unknown command") >> prompt

interpret :: Statement -> IO ()
interpret p = do
    result <- runRun $ exec p
    case result of
      Right ((), env) -> return ()
      Left exn        ->  System.print ("Uncaught exception: "++exn)

{-Test Programs-}
program2 :: Statement
program2 = foldl1 Seq [Assign "poo" (Const (I 5)),
                       Assign "var" (Const (I 1)),
                       Assign "poo" (Const (I 6)),
                       While (Gt (Var "poo") (Const (I 4))) (Print (Var "poo")),
                       Assign "poo" (Const (I 7)),
                       Print (Var "poo"),
                       Print (Var "var")]
