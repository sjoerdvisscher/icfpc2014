module CompileGHC (compileGHC) where

import Language.ECMAScript3.Parser
import Text.Parsec.Error
import Language.ECMAScript3.Syntax

compileGHC :: String -> IO String
compileGHC = compile . parseFromString

compile :: Either ParseError (JavaScript SourcePos) -> IO String
compile (Left err) = print err >> return ""
compile (Right (Script _ stmts)) = return "HLT"
