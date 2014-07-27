{-# LANGUAGE RecursiveDo #-}
module CompileGHC (compileGHC) where

import Language.ECMAScript3.Parser
import Text.Parsec.Error
import Language.ECMAScript3.Syntax

import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.Trans.RWS.Lazy

type Env = (Int, [(String, Int)])
type GHC = RWST Char () (Env, [Instr]) IO

push :: [Instr] -> GHC ()
push instrs = modify (second (++ instrs))

getOffset :: String -> GHC Int
getOffset s = do
  vars <- snd . fst <$> get
  maybe (fail ("Undeclared variable: " ++ s)) return $ lookup s vars

addVar :: String -> Int -> GHC Int
addVar nm size = do
  (offset, vars) <- fst <$> get
  modify (first (const (offset + size, (nm, offset):vars)))
  return offset

curPos :: GHC Int
curPos = do
  instrs <- snd <$> get
  return $ length instrs

goto :: Int -> GHC ()
goto lbl = push [JEQ lbl (Int 0) (Int 0)]

getReg :: GHC Arg
getReg = do
  c <- ask
  return $ Reg c

compileGHC :: String -> IO String
compileGHC = compile . parseFromString

compile :: Either ParseError (JavaScript SourcePos) -> IO String
compile (Left err) = print err >> return ""
compile (Right (Script _ stmts)) = do
  instrs <- snd . fst <$> execRWST (compileStmts stmts) 'a' ((0, []), [])
  return $ concatMap (\i -> show i ++ "\n") (instrs ++ [HLT])

compileStmts :: [Statement SourcePos] -> GHC ()
compileStmts = mapM_ compileStmt

compileStmt :: Statement SourcePos -> GHC ()
compileStmt (ExprStmt _ (AssignExpr _ OpAssign tgt src)) = do
  csrc <- compileExpr src
  lval <- compileLValue tgt
  assign lval csrc
compileStmt (ExprStmt _ (AssignExpr _ op tgt src)) = do
  (csrc:_) <- compileExpr src
  lval <- compileLValue tgt
  push [compileAssignOp op lval csrc]
compileStmt (ExprStmt _ e) = void (compileExpr e)
compileStmt (VarDeclStmt _ [VarDecl _ (Id _ s) Nothing]) = void (addVar s 1)
compileStmt (VarDeclStmt _ [VarDecl _ (Id _ s) (Just (NewExpr _ (VarRef _ (Id _ "Array")) [IntLit _ size]))]) = void (addVar s size)
compileStmt (VarDeclStmt _ [VarDecl _ (Id _ s) (Just (ArrayLit _ exprs))]) = do
  offset <- addVar s (length exprs)
  mapM_ (\(expr, ofs) -> compileExpr expr >>= assign (Ind (Int ofs))) $ zip exprs [offset..]
compileStmt (VarDeclStmt _ [VarDecl _ (Id _ s) (Just expr)]) = do
  cexpr <- compileExpr expr
  offset <- addVar s (length cexpr)
  assign (Ind (Int offset)) cexpr
compileStmt (IfStmt s1 (InfixExpr s2 OpLT l r) t e) = compileStmt (IfStmt s1 (InfixExpr s2 OpGEq l r) e t)
compileStmt (IfStmt s1 (InfixExpr s2 OpGT l r) t e) = compileStmt (IfStmt s1 (InfixExpr s2 OpLEq l r) e t)
compileStmt (IfStmt s1 (InfixExpr s2 OpEq l r) t e) = compileStmt (IfStmt s1 (InfixExpr s2 OpNEq l r) e t)
compileStmt (IfStmt _ (InfixExpr _ op l r) t e) = do
  rec
    (lArg:_) <- compileExpr l
    (rArg:_) <- local succ (compileExpr r)
    push [ compileCmpOp op afterThen lArg rArg ]
    compileStmt t
    goto afterElse
    afterThen <- curPos
    compileStmt e
    afterElse <- curPos
  return ()
compileStmt (IfSingleStmt s b t) = compileStmt (IfStmt s b t (BlockStmt s []))
compileStmt (BlockStmt _ stmts) = compileStmts stmts
compileStmt (ForStmt s (VarInit initS) (Just b) (Just inc) (BlockStmt _ stmts)) = do
  compileStmt (VarDeclStmt s initS)
  start <- curPos
  compileStmt (IfSingleStmt s b (BlockStmt s (stmts ++ [ExprStmt s inc, ThrowStmt s (IntLit s start)])))
compileStmt (ThrowStmt _ (IntLit _ start)) = goto start -- hack for ForStmt
compileStmt x = error ("Can't compile stmt: " ++ show x)

compileCmpOp :: InfixOp -> (Int -> Arg -> Arg -> Instr)
compileCmpOp OpGEq = JLT
compileCmpOp OpLEq = JGT
compileCmpOp OpNEq = JEQ
compileCmpOp x = error ("Can't compile cmp op: " ++ show x)

compileArithOp :: InfixOp -> (Arg -> Arg -> Instr)
compileArithOp OpAdd = ADD
compileArithOp OpSub = SUB
compileArithOp OpMul = MUL
compileArithOp OpDiv = DIV
compileArithOp OpBAnd = AND
compileArithOp OpBOr  = OR
compileArithOp OpBXor = XOR
compileArithOp x = error ("Can't compile arith op: " ++ show x)

compileAssignOp :: AssignOp -> (Arg -> Arg -> Instr)
compileAssignOp OpAssignAdd = ADD
compileAssignOp OpAssignSub = SUB
compileAssignOp OpAssignMul = MUL
compileAssignOp OpAssignDiv = DIV
compileAssignOp x = error ("Can't compile assign op: " ++ show x)

one :: Arg -> GHC [Arg]
one arg = return [arg]

two :: Arg -> Arg -> GHC [Arg]
two a b = return [a, b]

compileExpr :: Expression SourcePos -> GHC [Arg]
compileExpr (IntLit _ i) = one $ Int i
compileExpr (PrefixExpr _ PrefixMinus (IntLit _ i)) = one $ Int (256 - i)
compileExpr (VarRef _ (Id _ s)) = do
  offset <- getOffset s
  one $ Ind (Int offset)
compileExpr (BracketRef _ (VarRef _ (Id _ s)) (IntLit _ i)) = do
  offset <- getOffset s
  one $ Ind (Int (offset + i))
compileExpr (BracketRef p (VarRef _ (Id _ s)) expr) = do
  offset <- getOffset s
  (arg:_) <- compileExpr (InfixExpr p OpAdd (IntLit p offset) expr)
  one $ Ind arg
compileExpr (CallExpr _ (VarRef _ (Id _ s)) args) = do
  startReg <- ask
  mapM_ (\(c, arg) -> local (const c) (compileExpr arg) >>= assign (Reg c)) (zip [startReg..] args)
  push [INT (compileInterrupt s)]
  two ra rb
compileExpr (InfixExpr _ op l r) = do
  (lArg:_) <- compileExpr l
  (rArg:_) <- local succ (compileExpr r)
  case lArg of
    Reg{} -> do
      push [ compileArithOp op lArg rArg ]
      one lArg
    _ -> do
      reg <- getReg
      push [ MOV reg lArg, compileArithOp op reg rArg ]
      one reg
compileExpr (CondExpr p (InfixExpr _ OpLT l r) t e) = compileExpr (CondExpr p (InfixExpr p OpGEq l r) e t)
compileExpr (CondExpr p (InfixExpr _ OpGT l r) t e) = compileExpr (CondExpr p (InfixExpr p OpLEq l r) e t)
compileExpr (CondExpr p (InfixExpr _ OpEq l r) t e) = compileExpr (CondExpr p (InfixExpr p OpNEq l r) e t)
compileExpr (CondExpr _ (InfixExpr _ op l r) t e) = do
  reg <- getReg
  rec
    (lArg:_) <- compileExpr l
    (rArg:_) <- local succ (compileExpr r)
    push [ compileCmpOp op afterThen lArg rArg ]

    tArg <- compileExpr t
    assign reg tArg
    goto afterElse
    afterThen <- curPos

    eArg <- compileExpr e
    assign reg eArg
    afterElse <- curPos
  one reg
compileExpr x = error ("Can't compile expression: " ++ show x)

assign :: Arg -> [Arg] -> GHC ()
assign _ [] = return ()
assign tgt (src:srcs) = do
  when (tgt /= src) $ push [MOV tgt src]
  case tgt of
    Ind (Int offset) -> assign (Ind (Int (offset + 1))) srcs
    _ -> return ()

compileLValue :: LValue SourcePos -> GHC Arg
compileLValue (LVar _ s) = do
  offset <- getOffset s
  return $ Ind (Int offset)
compileLValue (LBracket _ (VarRef _ (Id _ s)) (IntLit _ i)) = do
  offset <- getOffset s
  return $ Ind (Int (offset + i))
compileLValue (LBracket p (VarRef _ (Id _ s)) expr) = do
  offset <- getOffset s
  (arg:_) <- compileExpr (InfixExpr p OpAdd (IntLit p offset) expr)
  return $ Ind arg
compileLValue x = error ("Can't compile lvalue: " ++ show x)

compileInterrupt :: String -> Int
compileInterrupt "move" = 0
compileInterrupt "lambdaPos" = 1
compileInterrupt "me" = 3
compileInterrupt "ghostPos" = 5
compileInterrupt "ghostStatus" = 6
compileInterrupt "map" = 7
compileInterrupt "debug" = 8
compileInterrupt x = error ("Can't compile interrupt: " ++ show x)

data Arg
  = Reg Char
  | PC
  | Int Int
  | Ind Arg
  deriving (Eq)

instance Show Arg where
  show (Reg c) = [c]
  show PC = "PC"
  show (Int i) = show i
  show (Ind arg) = "[" ++ show arg ++ "]"

ra, rb :: Arg
ra = Reg 'a'
rb = Reg 'b'
-- rc = Reg 'c'

data Instr
  = MOV Arg Arg
  | INC Arg
  | DEC Arg
  | ADD Arg Arg
  | SUB Arg Arg
  | MUL Arg Arg
  | DIV Arg Arg
  | AND Arg Arg
  | OR Arg Arg
  | XOR Arg Arg
  | JLT Int Arg Arg
  | JEQ Int Arg Arg
  | JGT Int Arg Arg
  | INT Int
  | HLT
  deriving (Eq)

instance Show Instr where
  show (MOV a b) = "MOV " ++ show a ++ ", " ++ show b
  show (INC a) = "INC " ++ show a
  show (DEC a) = "DEC " ++ show a
  show (ADD a b) = "ADD " ++ show a ++ ", " ++ show b
  show (SUB a b) = "SUB " ++ show a ++ ", " ++ show b
  show (MUL a b) = "MUL " ++ show a ++ ", " ++ show b
  show (DIV a b) = "DIV " ++ show a ++ ", " ++ show b
  show (AND a b) = "AND " ++ show a ++ ", " ++ show b
  show (OR  a b) = "OR  " ++ show a ++ ", " ++ show b
  show (XOR a b) = "XOR " ++ show a ++ ", " ++ show b
  show (JLT t a b) = "JLT " ++ show t ++ ", " ++ show a ++ ", " ++ show b
  show (JEQ t a b) = "JEQ " ++ show t ++ ", " ++ show a ++ ", " ++ show b
  show (JGT t a b) = "JGT " ++ show t ++ ", " ++ show a ++ ", " ++ show b
  show (INT a) = "INT " ++ show a
  show HLT = "HLT"
