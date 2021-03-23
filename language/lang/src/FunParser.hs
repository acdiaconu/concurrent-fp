{-# OPTIONS_GHC -fno-warn-tabs #-}
module FunParser(funParser) where
import Parsing
import FunSyntax
import Data.Char
import Data.Set (fromList, isSubsetOf)

import Debug.Trace

data Token = 
    IDENT IdKind Ident | NUMBER Integer | STRING String
  | LPAR | RPAR | COMMA | EQUAL | ASSIGN | SEMI | SSEMI
  | IF | THEN | ELSE | LET | REC | VAL | LAMBDA | IN | WHILE | DO
  | PIPE | MINUS | STAR | AMPER | ARRAY | BRA | KET | DATA | MATCH | WITH
  | LBRACE | RBRACE | ARROW | VBAR | DOT | OPEN | PAR | SEND | RECV | NEWCH
  | BADTOK Char | THROW | CLOSE | CATCH
  deriving Eq

data IdKind = 
  ID | MONOP | CONSOP | MULOP | ADDOP | RELOP 
  deriving (Eq, Show)

instance Show Token where
  show t = 
    case t of 
      IDENT k x -> show x; NUMBER n -> show n; 
      STRING s -> "\"" ++ s ++ "\""
      LPAR -> "("; RPAR -> ")"; COMMA -> ","
      EQUAL -> "="; SEMI -> ";"; SSEMI -> ";;"; ASSIGN -> ":="
      IF -> "if"; THEN -> "then"; ELSE -> "else"; LET -> "let"
      REC -> "rec"; VAL -> "val"; LAMBDA -> "fun"; IN -> "in"
      WHILE -> "while"; DO -> "do"; PIPE -> ">>"
      MINUS -> "-"; STAR -> "*"; AMPER -> "&"; ARRAY -> "array"
      BRA -> "["; KET -> "]"; LBRACE -> "{"; RBRACE -> "}"
      ARROW -> "=>"; VBAR -> "|"; DOT -> "."; OPEN -> "open"; PAR -> "||"
      SEND -> "!"; RECV -> "?"; NEWCH -> "newChan"; DATA -> "data";
      MATCH -> "match"; WITH -> "with"; CATCH -> "catch"
      CLOSE -> "close"; THROW -> "throw"; BADTOK c -> [c]

kwlookup = 
  make_kwlookup (IDENT ID)
    [("if", IF), ("then", THEN), ("else", ELSE), ("let", LET), ("in", IN),
      ("rec", REC), ("val", VAL), ("fun", LAMBDA), ("while", WHILE), 
      ("do", DO), ("array", ARRAY), ("open", OPEN),
      ("div", IDENT MULOP "div"), ("mod", IDENT MULOP "mod"), 
      ("newChan", NEWCH), ("data", DATA), ("match", MATCH), ("with", WITH),
      ("throw", THROW), ("close", CLOSE), ("catch", CATCH)]

lexer =
  do 
    c <- nextch
    case c of
      _ | isAlpha c ->
        do 
          s <- star (\ c -> isAlphaNum c || c == '_')
          return (kwlookup (c:s))
      _ | isDigit c ->
        do s <- star isDigit; return (NUMBER (read (c:s)))
      '"' ->
        do s <- star (/= '"'); nextch; return (STRING s)
      '=' -> switch [('>', return ARROW)] (return EQUAL)
      '+' -> return (IDENT ADDOP "+")
      '-' -> switch [('-', do scanComment; lexer)] (return MINUS)
      '?' -> return RECV
      '!' -> return SEND
      '*' -> return STAR
      '&' -> return AMPER
      '~' -> return (IDENT MONOP "~")
      ',' -> return COMMA
      '<' -> switch [('=', return (IDENT RELOP "<=")),
                        ('>', return (IDENT RELOP "<>"))]
                (return (IDENT RELOP "<"))
      '>' -> switch [('=', return (IDENT RELOP ">=")),
			('>', return PIPE)]
                (return (IDENT RELOP ">"))
      '(' -> return LPAR
      ')' -> return RPAR
      '[' -> return BRA
      ']' -> return KET
      '{' -> return LBRACE
      '}' -> return RBRACE
      '.' -> return DOT
      '|' -> switch [('|', return PAR)] (return VBAR)
      ';' -> switch [(';', return SSEMI)] (return SEMI)
      ':' -> switch [('=', return ASSIGN)] 
		(return (IDENT CONSOP ":"))
      ' ' -> lexer
      '\t' -> lexer 
      '\n' -> do incln; lexer
      _ -> return (BADTOK c)
              
scanComment =
  do 
    c <- nextch
    case c of
      '\n' -> incln
      _ -> scanComment

p_phrase =
  do e <- p_expr; eat SSEMI; return (Calculate e)
  <+> do d <- p_def; eat SSEMI; return (Define d)

p_def = 
  do eat VAL; (x, e) <- p_eqn; return (Val x e)
  <+> do eat REC; (x, e) <- p_eqn; return (Rec x e)
  <+> do eat DATA; (x, ctors) <- p_seqdef; return (Data x ctors) 

p_eqn =
  do x <- p_name; eat EQUAL; e <- p_expr; return (x, e)
  <+> do x <- p_name; xs <- p_formals; eat EQUAL; e <- p_expr; 
         return (x, nested_lam xs e)

-- TODO: add prefix for constructor to be data name

p_seqdef = do x <- p_name; xs <- p_formals; eat EQUAL; 
              ctors <- p_seq $ p_ctor xs; 
              return (x, ctors)

p_ctor allowed = do x <- p_name; xs <- p_formals; eat VBAR;
                    case isSubsetOf (fromList xs) (fromList allowed) of
                      False -> p_fail
                      True -> let args = genArgs (length xs) in 
                                return (Val x (nested_lam args (Injector x (map Variable args))))
  <+> do x <- p_name; xs <- p_formals;
                    case isSubsetOf (fromList xs) (fromList allowed) of
                      False -> p_fail
                      True -> let args = genArgs (length xs) in 
                                return (Val x (nested_lam args (Injector x (map Variable args))))

genArgs n = take n (map (\n -> "x" ++ show n) [1 ..]) 

nested_lam :: [Ident] -> Expr -> Expr
nested_lam [x] e = Lambda x e
nested_lam (x:xs) e = Lambda x (nested_lam xs e)

p_formals = 
  do xs <- p_seq p_name; return xs

p_expr = 
  do eat LET; d <- p_def; eat IN; e1 <- p_expr; return (Let d e1)
  <+> do eat LAMBDA; xs <- p_formals; 
		e1 <- p_expr; return (nested_lam xs e1)
  <+> do eat MATCH; ex <- p_expr; eat WITH; pats <- p_seq p_pattern;
         return (Match ex pats) 
  <+> do eat CATCH; ex <- p_expr; eat WITH; pats <- p_seq p_pattern;
         return (TryCatch ex pats)
  <+> p_par

p_pattern = do p <- p_patctor; eat ARROW; ex <- p_expr; eat VBAR;
               return (Pattern p ex)
  <+> do p <- p_patctor; eat ARROW; ex <- p_expr; 
         return (Pattern p ex)

p_patctor = do ct <- p_name; vars <- p_seq p_name; 
               return (VarCtor ct vars) 

p_par =
  do es <- p_list p_cond PAR; 
     case es of
       [x] -> return x
       xs  -> return (Parallel xs)
  <+> p_cond

p_cond = 
  do eat IF; e1 <- p_cond; eat THEN; e2 <- p_cond;
     eat ELSE; e3 <- p_cond; return (If e1 e2 e3)
  <+> p_term6

p_term6 = 
  p_chainr mk (eat PIPE) p_term5
  where
    mk PIPE e1 e2 = Pipe e1 e2

p_term5 = p_opchainl p_relop p_term4 
p_term4 = p_opchainl p_addop p_term3
p_term3 = p_opchainl p_mulop p_term1

p_relop = p_ident RELOP <+> (do eat EQUAL; return "=")
p_addop = p_ident ADDOP <+> (do eat MINUS; return "-")
p_mulop = p_ident MULOP <+> (do eat STAR; return "*")

p_opchainl :: Parser t Ident -> Parser t Expr -> Parser t Expr
p_opchainl p_op p_rand = 
  do e0 <- p_rand; p_tail e0
  where
    p_tail e1 =
      do w <- p_op; e2 <- p_rand; case w of 
           "+"   -> p_tail (BinPrim Plus e1 e2)
           "-"   -> p_tail (BinPrim Minus e1 e2)
           "*"   -> p_tail (BinPrim Times e1 e2)
           "div" -> p_tail (BinPrim Div e1 e2)
           "mod" -> p_tail (BinPrim Mod e1 e2)
           "="   -> p_tail (BinPrim Equal e1 e2)
           "and" -> p_tail (BinPrim And e1 e2)
           "or"  -> p_tail (BinPrim Or e1 e2)
           _     -> p_tail (Apply (Apply (Variable w) e1) e2)
      <+> return e1

p_chainl ::(a -> b -> b -> b) -> Parser t a -> Parser t b -> Parser t b
p_chainl mk p_op p_rand = 
  do e0 <- p_rand; p_tail e0
  where
    p_tail e1 =
      do w <- p_op; e2 <- p_rand; p_tail (mk w e1 e2)
      <+> return e1

p_chainr :: (a -> b -> b -> b) -> 
    Parser t a -> Parser t b -> Parser t b 
p_chainr mk p_op p_rand =
  do e1 <- p_rand; p_tail e1
  where
    p_tail e1 =
      do w <- p_op; e2 <- p_chainr mk p_op p_rand; 
					return (mk w e1 e2)
      <+> return e1

p_term1 =
  do w <- p_monop; e <- p_term1; return (MonPrim w e)
  <+> do ce <- p_primary; eat RECV; return (Receive ce) 
  <+> do ce <- p_primary; eat SEND; ve <- p_primary; return (Send ce ve) 
  <+> do eat NEWCH; return NewChan 
  <+> do eat CLOSE; e <- p_term1; return (Close e)
  <+> do eat THROW; e <- p_term1; return (Throw e)
  <+> p_term0

p_monop = (do eat MINUS; return Neg);

p_term0 =
  do e0 <- p_primary; p_qualifiers e0;
  where
    p_qualifiers e1 =
      do ap <- p_primary; p_qualifiers (Apply e1 ap)
      <+> return e1

p_primary =
  do n <- p_number; return (Number n)
  <+> do x <- p_name; return (Variable x)
  <+> do eat LPAR; e <- p_expr; eat RPAR; return e

p_base =
  (do e <- p_expr; eat VBAR; return e)

p_binding =
  do x <- p_name; eat ARROW; e <- p_expr; return (x, e)

p_number =
  do t <- scan; case t of NUMBER n -> return n; _ -> p_fail

p_string =
  do t <- scan; case t of STRING s -> return s; _ -> p_fail

p_name = p_ident ID <+> (do eat LPAR; x <- p_op; eat RPAR; return x)

p_op =
  p_ident MONOP <+> p_addop <+> p_mulop <+> p_relop

p_ident k =
  do t <- scan; case t of IDENT k' x | k == k' -> return x; _ -> p_fail

funParser :: Syntax Token Phrase
funParser = (lexer, p_phrase)