module George.Syntax where

data Program = Program { inputTy  :: Ty
                       , outputTy :: Ty
                       , resultTy :: Ty
                       , funDefns :: [FunDefn]
                       }
               deriving Show

data Ty = Bit | BitVector Int | Pause deriving Show

data FunDefn = FunDefn { defnName   :: Ident
                       , defnParams :: [(Ident,Ty)]
                       , defnRetTy  :: Ty
                       , defnBody   :: Expr
                       }
               deriving Show

type Ident = String

data Prim = PrimFIXME deriving Show

data Expr = Yield Expr Ident [Expr]
          | Terminate Expr
          | Let Ident Expr Expr
          | BitConst Bit
          | BitVectorConst [Bit]
          | BitVectorSlice Expr Int Int
          | BitVectorSelect Expr Int
          | Primitive Prim [Expr]
          | FunVarCall Ident [Expr]
          | IfThenElse Expr Expr Expr
          deriving Show

data Bit = Zero | One
           deriving Show
