{-# LANGUAGE RecursiveDo #-}
module CompileGCC (compileGCC) where

import Language.Haskell.Parser
import Language.Haskell.Syntax

import Control.Applicative
import Control.Monad.Trans.RWS.Lazy
import Data.List

compileGCC :: String -> IO String
compileGCC = compile . parseModule

compile :: ParseResult HsModule -> IO String
compile err@ParseFailed{} = print err >> return ""
compile (ParseOk (HsModule _ _ _ _ decls)) = do
  rec
    fns <- fixOrder [] . snd <$> evalRWST (mapM_ compileDecl decls) (Env positions) (0, 0)
    let positions = snd $ mapAccumL (\p (nm, instrs) -> (p + length instrs, (nm, LDF p))) 0 fns
  return $ concatMap (\(_, body) -> concatMap (('\n' :) . show') body) fns

fixOrder :: [(String, a)] -> [(String, a)] -> [(String, a)]
fixOrder acc (("main", b):rest) = ("main", b):(acc ++ rest)
fixOrder acc (p:rest) = fixOrder (acc ++ [p]) rest
fixOrder acc [] = acc

type GCC = RWST Env [(String, [Instr])] (Int, Int) IO

type Ctx = [(String, Instr)]
data Env = Env { vars :: Ctx }

data Instr
  = LDC Int
  | LD Int Int
  | ADD
  | SUB
  | MUL
  | DIV
  | CEQ
  | CGT
  | CGTE
  | ATOM
  | CONS
  | CAR
  | CDR
  | SEL Int Int
  | JOIN
  | LDF Int
  | AP Int
  | RTN
  | DUM Int
  | RAP Int
  | STOP
  | TSEL Int Int
  | TAP Int
  | TRAP Int
  | ST Int Int
  | DBUG
  deriving (Show)

show' :: Instr -> String
show' (LDC i) = "LDC " ++ show i
show' x = show x

createName :: GCC String
createName = do
  (i, c) <- get
  put (i + 1, c)
  return $ show i

count :: Int -> GCC a -> GCC a
count n m = do
  (i, c) <- get
  put (i, c + n)
  m

push :: Instr -> GCC [Instr]
push instr = count 1 $ return [instr]

curPos :: GCC Int
curPos = do
  (_, c) <- get
  return c

(<++>) :: GCC [a] -> GCC [a] -> GCC [a]
(<++>) = liftA2 (++)

compileDecl :: HsDecl -> GCC ()
compileDecl (HsFunBind [HsMatch _ ident args (HsUnGuardedRhs body) []]) = compileFn (getName ident) args body
compileDecl x = error ("Can't compile fn: " ++ show x)

compileFn :: String -> [HsPat] -> HsExp -> GCC ()
compileFn nm args body = do
    instrs <- withArgs [ (arg, LD 0 i) | (HsPVar (HsIdent arg), i) <- zip args [0..]] body
    tell [(nm, instrs)]

withArgs :: [(String, Instr)] -> HsExp -> GCC [Instr]
withArgs env' expr = local fixEnv (compileExpr expr) <++> push RTN
  where
    fixEnv :: Env -> Env
    fixEnv (Env env) = Env (env' ++ map (fmap incFrame) env)
    incFrame :: Instr -> Instr
    incFrame (LD fr i) = LD (fr + 1) i
    incFrame x = x

compileExpr :: HsExp -> GCC [Instr]
compileExpr (HsTuple exprs) = foldr1 (\l r -> l <++> r <++> push CONS) $ map compileExpr exprs
compileExpr (HsList exprs) = foldr (\l r -> compileExpr l <++> r <++> push CONS) (push $ LDC 0) exprs
compileExpr (HsLit (HsInt i)) = push $ LDC (fromInteger i)
compileExpr (HsNegApp (HsLit (HsInt i))) = push $ LDC (-fromInteger i)
compileExpr (HsVar (UnQual (HsIdent s))) = compileVar s 0
compileExpr (HsInfixApp l op r) = do
  cl <- compileExpr l
  cr <- compileExpr r
  cop <- compileVar (getOpName op) 2
  return $ cl ++ cr ++ cop
compileExpr (HsLet [HsPatBind _ var (HsUnGuardedRhs expr) []] body) = do
  cexpr <- compileExpr expr
  nm <- createName
  compileFn nm [var] body
  call <- compileVar nm 1
  return $ cexpr ++ call
compileExpr (HsIf b l r) = do
  lnm <- createName
  cl <- compileExpr l <++> push JOIN
  tell [(lnm, cl)]
  rnm <- createName
  cr <- compileExpr r <++> push JOIN
  tell [(rnm, cr)]
  compileExpr b <++> ((\env -> [case (lookup lnm (vars env), lookup rnm (vars env)) of
      (Just (LDF pl), Just (LDF pr)) -> SEL pl pr
      _ -> STOP
    ]) <$> count 1 ask)
compileExpr (HsLambda _ args body) = do
  nm <- createName
  compileFn nm args body
  compileVar nm 0
compileExpr e@HsApp{} = compileApp e []
compileExpr (HsParen e) = compileExpr e
compileExpr x = fail ("Can't compile expr: " ++ show x)

compileApp :: HsExp -> [GCC [Instr]] -> GCC [Instr]
compileApp (HsVar (UnQual (HsIdent fn))) args = (concat <$> sequence args) <++> compileVar fn (length args)
compileApp (HsApp fn arg) args = compileApp fn (compileExpr arg : args)
compileApp (HsParen e) args = compileApp e args
compileApp x _ = fail ("Can't compile app: " ++ show x)

compileVar :: String -> Int -> GCC [Instr]
compileVar ":" _ = push CONS
compileVar "+" _ = push ADD
compileVar "-" _ = push SUB
compileVar "*" _ = push MUL
compileVar "/" _ = push DIV
compileVar "==" _ = push CEQ
compileVar ">" _ = push CGT
compileVar ">=" _ = push CGTE
compileVar "car" _ = push CAR
compileVar "cdr" _ = push CDR
compileVar "fst" _ = push CAR
compileVar "snd" _ = push CDR
compileVar "debug" _ = push DBUG
compileVar "atom" _ = push ATOM
compileVar s 0 = (\env -> [maybe (error ("Undefined variable: " ++ s)) id $ lookup s (vars env)]) <$> count 1 ask
compileVar s n = (\env -> [maybe (error ("Undefined variable: " ++ s)) id $ lookup s (vars env), AP n]) <$> count 2 ask

getOpName :: HsQOp -> String
getOpName (HsQVarOp op) = getQName op
getOpName (HsQConOp op) = getQName op

getQName :: HsQName -> String
getQName (Qual _ nm) = getName nm
getQName (UnQual nm) = getName nm
getQName (Special s) = getSpecialName s

getName :: HsName -> String
getName (HsIdent nm) = nm
getName (HsSymbol nm) = nm

getSpecialName :: HsSpecialCon -> String
getSpecialName HsCons = ":"
getSpecialName x = error ("Unsupported special name: " ++ show x)
