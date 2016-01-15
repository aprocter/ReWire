module George.PrettyPrinter where

import George.Syntax
import Text.PrettyPrint

class Pretty a where
  pretty :: a -> Doc

instance Pretty Program where
  pretty (Program t_i t_o t_r fds) = (text "INPUT" <+> pretty t_i <> semi)
                                  $$ (text "OUTPUT" <+> pretty t_o <> semi)
                                  $$ (text "RESULT" <+> pretty t_r <> semi)
                                  $$ vcat (map pretty fds)

instance Pretty Ty where
  pretty Bit           = text "bit"
  pretty (BitVector n) = text "bit" <> char '<' <> int n <> char '>'
  pretty Pause         = text "pause"

instance Pretty FunDefn where
  pretty (FunDefn n ps t_r e) = text n
                                  <+> case ps of { [] -> empty ; _ -> parens $ sep $ punctuate comma $ map p_par ps }
                                  <+> colon
                                  <+> pretty t_r
                                  <+> equals
                                  $$ (nest 4 (pretty e) <> semi)
     where p_par (n,t) = text n <+> colon <+> pretty t

instance Pretty Expr where
  pretty (Yield e n es)         = text "yield" <+> pretty e <+> text n <+>
                                   (case es of
                                      [] -> empty
                                      _  -> parens $ sep $ punctuate comma $ map pretty es)
  pretty (Terminate e)          = text "terminate" <+> pretty e
  pretty (Let n e e')           = parens (hang (text "let") 4 (text n <+> equals <+> pretty e)
                                          $$ hang (text "in") 4 (pretty e'))
  pretty (BitConst b)           = pretty b
  pretty (BitVectorConst bs)    = char '<' <> cat (map pretty bs) <> char '>'
  pretty (BitVectorSlice e l r) = text "slice" <+> pretty e <+> brackets (int l <> colon <> int r)
  pretty (BitVectorSelect e i)  = text "select" <+> pretty e <+> brackets (int i)
  pretty (Primitive _ _)        = text "FIXME:PRIMITIVE"
  pretty (FunVarCall n es)      = text n <+> case es of
                                               [] -> empty
                                               _  -> parens $ sep $ punctuate comma $ map pretty es
  pretty (IfThenElse e e_t e_f) = parens ((text "if" <+> pretty e)
                                          $$ (hang (text "then") 4 (pretty e_t))
                                          $$ (hang (text "else") 4 (pretty e_f)))

instance Pretty Bit where
  pretty Zero = text "0"
  pretty One  = text "1"
