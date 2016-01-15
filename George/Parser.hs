module George.Parser (parseGeorge,parseGeorgeFile) where

import Control.Monad.Identity
import George.Syntax
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok

--
-- Parsec boilerplate.
--
type GPM = ParsecT String () Identity

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser (javaStyle { Tok.reservedNames   = ["yield","terminate","let","in","slice","select","if","then","else","bit","pause","INPUT","OUTPUT","RESULT"]
                                       , Tok.reservedOpNames = ["=",":"]
                                       , Tok.caseSensitive   = True })

identifier,_operator :: GPM String
identifier  = Tok.identifier lexer
_operator   = Tok.operator lexer

reserved,reservedOp :: String -> GPM ()
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

_charLiteral :: GPM Char
_charLiteral = Tok.charLiteral lexer

_stringLiteral :: GPM String
_stringLiteral = Tok.stringLiteral lexer

natural,_integer,_decimal,_hexadecimal,_octal :: GPM Integer
natural      = Tok.natural lexer
_integer     = Tok.integer lexer
_decimal     = Tok.decimal lexer
_hexadecimal = Tok.hexadecimal lexer
_octal       = Tok.octal lexer

_float :: GPM Double
_float = Tok.float lexer

_naturalOrFloat :: GPM (Either Integer Double)
_naturalOrFloat = Tok.naturalOrFloat lexer

symbol :: String -> GPM String
symbol = Tok.symbol lexer

lexeme :: GPM a -> GPM a
lexeme = Tok.lexeme lexer

whiteSpace :: GPM ()
whiteSpace = Tok.whiteSpace lexer

parens,_braces,angles,brackets :: GPM a -> GPM a
parens   = Tok.parens lexer
_braces  = Tok.braces lexer
angles   = Tok.angles lexer
brackets = Tok.brackets lexer

semi,_comma,_colon,_dot :: GPM String
semi   = Tok.semi lexer
_comma = Tok.comma lexer
_colon = Tok.colon lexer
_dot   = Tok.dot lexer

_semiSep,_semiSep1,commaSep,_commaSep1 :: GPM a -> GPM [a]
_semiSep   = Tok.semiSep lexer
_semiSep1  = Tok.semiSep1 lexer
commaSep   = Tok.commaSep lexer
_commaSep1 = Tok.commaSep1 lexer
--
-- End Parsec boilerplate.
--

parseGeorgeFile :: FilePath -> IO (Either ParseError Program)
parseGeorgeFile fp = readFile fp >>= return . runParser top () fp

parseGeorge :: String -> Either ParseError Program
parseGeorge = runParser top () ""

top :: GPM Program
top = do whiteSpace
         p <- program
         eof
         return p

program :: GPM Program
program = do reserved "INPUT"
             reservedOp ":"
             t_i <- ty
             _   <- semi

             reserved "OUTPUT"
             reservedOp ":"
             t_o <- ty
             _   <- semi

             reserved "RESULT"
             reservedOp ":"
             t_r <- ty
             _   <- semi

             fds <- many fundefn

             return (Program { inputTy  = t_i
                             , outputTy = t_o
                             , resultTy = t_r
                             , funDefns = fds })

ty :: GPM Ty
ty = (do reserved "bit"
         mwidth <- optionMaybe (brackets natural)
         return (maybe Bit (BitVector . fromInteger) mwidth))
 <|> (reserved "pause" >> return Pause)

fundefn :: GPM FunDefn
fundefn = do n      <- identifier

             params <- option [] (parens (commaSep param))
             
             reserved ":"
             t_r    <- ty
             
             reserved "="
             e      <- expr

             _      <- semi
             
             return (FunDefn { defnName   = n
                             , defnParams = params
                             , defnRetTy  = t_r
                             , defnBody   = e })

param :: GPM (Ident,Ty)
param = do n <- identifier
           reservedOp ":"
           t <- ty
           return (n,t)

expr :: GPM Expr
expr = (do reserved "yield"
           e  <- expr
           n  <- identifier
           es <- option [] (parens (commaSep expr))
           return (Yield e n es))
   <|> (do reserved "terminate"
           e <- expr
           return (Terminate e))
   <|> (do reserved "let"
           n  <- identifier
           reservedOp "="
           e  <- expr
           reservedOp "in"
           e' <- expr
           return (Let n e e'))
   <|> (do b <- bit
           return (BitConst b))
   <|> (do bs <- angles (many bit)
           return (BitVectorConst bs))
   <|> (do reserved "slice"
           e     <- expr
           (l,r) <- brackets $ do
                      l <- natural
                      reservedOp ":"
                      r <- natural
                      return (l,r)
           return (BitVectorSlice e (fromInteger l) (fromInteger r)))
   <|> (do reserved "select"
           e <- expr
           i <- brackets natural
           return (BitVectorSelect e (fromInteger i)))
   <|> (do n  <- identifier
           es <- option [] (parens (commaSep expr))
           return (FunVarCall n es))
   <|> (do reserved "if"
           e   <- expr
           reserved "then"
           e_t <- expr
           reserved "else"
           e_f <- expr
           return (IfThenElse e e_t e_f))
   <|> parens expr
   <?> "expression"

bit :: GPM Bit
bit = (lexeme (symbol "0") >> return Zero) <|> (lexeme (symbol "1") >> return One) <?> "bit"
