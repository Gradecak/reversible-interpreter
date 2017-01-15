{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Interpreter (help, interpret, Statement) where

import           Control.Monad          (void, when)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map               as Map
import           Data.Maybe
import           Prelude                hiding (lookup, print)
import           System.Exit            (exitSuccess)
import qualified System.IO              as System

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
               Nothing -> fail ("Unknown variable "++ k)

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

type Run a = StateT [(Env, Statement)] (ExceptT String IO) a
runRun p =  runExceptT (runStateT p [(Map.empty, Pass)])

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

exec (If cond s0 s1) = do
    ((env,_):_) <- get
    Right (B val) <- return $ runEval env (eval cond)
    if val then exec s0 else exec s1

exec (Try s0 s1) = catchError (exec s0) (\e -> exec s1)
exec (Seq s0 s1) = exec s0 >> exec s1
exec Pass = return ()

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
    x <- get
    if length x == 1
      then liftIO (putStrLn "no further undo") >> prompt s
      else do let ((_,statement):xs) = x -- pattern match current state of the program
              put xs                     -- rollback state by 1 entry
              exec statement             -- exec rolled back statement (will result in current state)
              prompt s

-- inspect the current state of a variable in the Env
inspectCurrent :: Name -> Run ()
inspectCurrent name = do
    ((env,_):_) <- get -- pattern match current state
    let x = Map.lookup name env
    liftIO $ case x of
      (Just val) -> pPrintVar name val
      Nothing    -> putStrLn "error"

-- inspect the state of a variable across all of the enviroments
inspectAll :: Name -> Run ()
inspectAll name = do
    states <- get
    let envs = map fst states                  -- change [(Env, Statement)] to [Env]
        x    = mapMaybe (Map.lookup name) envs -- perform a lookup on our enviroments
    liftIO $ mapM_ (pPrintVar name) x          -- pretty print all of the past values

prompt :: Statement -> Run ()
prompt s = do
    liftIO $ putStrLn $ show s ++ " ?"  -- show statement to be executed
    p                                   -- prompt user for input
  where p = do
            liftIO $ putStr ">"
            x <- liftIO $ words <$> getLine
            case head' x of
              Nothing  -> void $ saveCurrentState s
              Just "q" -> void $ liftIO exitSuccess
              Just "i" -> inspectCurrent (last x) >> p
              Just "b" -> stepback s
              Just "I" -> inspectAll (last x) >> p
              _        -> liftIO help >> p

interpret :: Statement -> IO ()
interpret p = do
    help
    ((), x) <- runErr $ analyse p
    showWarnings x
    result <- runRun $ exec p
    case result of
      Right ((), env) -> return ()
      Left exn        -> System.print ("Uncaught exception: "++exn)

help :: IO ()
help =  putStrLn "\ESC[34m (RET)step | (b)ack  | (i)nspect <name> | (I)nspectAll <name> | (q)uit \ESC[0m"

{------------------ Static Analysis Section -----------------------------------------}

-- we will use variable states to check if a variable is used
data VarState = Used   -- Variable Was initialised and Used
              | Init   -- Variable has been initialsed but not used
              deriving (Eq)

type VarStates = Map.Map Name VarState

type Err a = StateT VarStates IO a
runErr p = runStateT p Map.empty

analyse :: Statement -> Err ()
analyse (Assign name v) = do
    env <- get
    exprCheck v
    state $ \s -> ((), Map.insert name Init s) -- set var status to initialised

analyse s@(While expr s0) = exprCheck expr >>= showError s >> analyse s0
analyse s@(If expr s0 s1) = exprCheck expr >>= showError s >> analyse s0 >> analyse s1
analyse s@(Print expr) = exprCheck expr >>= showError s
analyse (Seq s0 s1) = analyse s0 >> analyse s1
analyse (Try s0 s1) = analyse s0 >> analyse s1
analyse Pass = return () -- for completeness sake

-- we are only interested in expressions where a variable is involved
-- if only pattern matching on _ for Constructors was an option... :(
exprCheck :: Expr -> Err (Either String ())
exprCheck (Add e0 e1) = exprCheck e0 >> exprCheck e1
exprCheck (Sub e0 e1) = exprCheck e0 >> exprCheck e1
exprCheck (Mul e0 e1) = exprCheck e0 >> exprCheck e1
exprCheck (Div e0 e1) = exprCheck e0 >> exprCheck e1
exprCheck (And e0 e1) = exprCheck e0 >> exprCheck e1
exprCheck (Or e0 e1)  = exprCheck e0 >> exprCheck e1
exprCheck (Eq e0 e1)  = exprCheck e0 >> exprCheck e1
exprCheck (Gt e0 e1)  = exprCheck e0 >> exprCheck e1
exprCheck (Lt e0 e1)  = exprCheck e0 >> exprCheck e1
exprCheck (Not e)     = exprCheck e
exprCheck (Const _)   = return $ Right () -- not needed, here for completeness
exprCheck (Var name) = do
    states <- get
    case Map.lookup name states of
      Just x -> state $ \s -> (Right (), stateTrans name x s)
      Nothing -> return $ Left $ "\ESC[31m ERROR: Variable " ++ show name ++ " used before initialisation\ESC[0m"

-- if a var is in the 'init' state, it means we are using it so we change its state to 'used'
-- if a var is in the 'used' state we leave it there
stateTrans :: Name -> VarState -> VarStates -> VarStates
stateTrans n Init env = Map.insert n Used env
stateTrans _ _ env = env

showError :: Statement -> Either String () -> Err ()
showError s (Left e)  = void $ liftIO (putStrLn $ e ++ " @ " ++ show s)
showError _ (Right _) = return ()

showWarnings :: VarStates -> IO ()
showWarnings states = do
    let x = Map.filter (/= Used) states -- find all of the variables still in 'Init' state
        y = Map.toList x
    mapM_ warningMsg y

warningMsg :: (Name, VarState) -> IO ()
warningMsg (n,_) = putStrLn ("\ESC[33m WARNING: " ++ show n ++ " initialsed but never used\ESC[0m")
