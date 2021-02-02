{-# OPTIONS_GHC -fno-warn-tabs #-}
module FunParser(funParser) where
import Parsing
import FunSyntax
import Data.Char
import Debug.Trace

data Token = 
    IDENT IdKind Ident | NUMBER Integer | STRING String
  | LPAR | RPAR | COMMA | EQUAL | ASSIGN | SEMI | SSEMI
  | IF | THEN | ELSE | LET | REC | VAL | LAMBDA | IN | WHILE | DO
  | ORELSE | PIPE | MINUS | STAR | AMPER | ARRAY | BRA | KET 
  | LBRACE | RBRACE | ARROW | VBAR | DOT | OPEN | PAR | SEND | RECV | NEWCH
  | BADTOK Char
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
      REC -> "rec"; VAL -> "val"; LAMBDA -> "lambda"; IN -> "in"
      WHILE -> "while"; DO -> "do"; ORELSE -> "orelse"; PIPE -> ">>"
      MINUS -> "-"; STAR -> "*"; AMPER -> "&"; ARRAY -> "array"
      BRA -> "["; KET -> "]"; LBRACE -> "{"; RBRACE -> "}"
      ARROW -> "=>"; VBAR -> "|"; DOT -> "."; OPEN -> "open"; PAR -> "||"
      SEND -> "!"; RECV -> "?"; NEWCH -> "newChan"
      BADTOK c -> [c]

kwlookup = 
  make_kwlookup (IDENT ID)
    [("if", IF), ("then", THEN), ("else", ELSE), ("let", LET), ("in", IN),
      ("rec", REC), ("val", VAL), ("lambda", LAMBDA), ("while", WHILE), 
      ("do", DO), ("orelse", ORELSE), ("array", ARRAY), ("open", OPEN),
      ("div", IDENT MULOP "div"), ("mod", IDENT MULOP "mod"), ("newChan", NEWCH)]

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

-- {\syn _phrase_ \arrow\ _expr_ ";;" \orr\ _def_ ";;"}
p_phrase =
  do e <- p_expr; eat SSEMI; return (Calculate e)
  <+> do d <- p_def; eat SSEMI; return (Define d)

-- {\syn _def_ \arrow\ "val" _eqn_ \orr\ "rec" _eqn_ \orr\ "array" _name_ "[" _expr_ "]" \orr\ "open" _expr_}
p_def = 
  do eat VAL; (x, e) <- p_eqn; return (Val x e)
  <+> do eat REC; (x, e) <- p_eqn; return (Rec x e)

-- {\syn _eqn_ \arrow\ _name_ "=" _expr_ \orr\ _name_ _formals_ "=" _expr_}
p_eqn =
  do x <- p_name; eat EQUAL; e <- p_expr; return (x, e)
  <+> do x <- p_name; xs <- p_formals; 
		eat EQUAL; e <- p_expr; return (x, nested_lam xs e)

nested_lam :: [Ident] -> Expr -> Expr
nested_lam [x] e = Lambda x e
nested_lam (x:xs) e = Lambda x (nested_lam xs e)

-- {\syn _formals_ \arrow\ "(" \[ _ident_ \{ "," _ident_ \} \] ")"}
p_formals = 
  do xs <- p_seq p_name; return xs

-- {\syn expr \arrow\ "let" _def_ "in" _expr_ \orr\ "lambda" _formals_ _expr_} 
-- {\syn \qquad\qquad\qquad \orr\ "fix" "(" _ident_ ")" _expr_ \orr\ _sequence_}
p_expr = 
  do eat LET; d <- p_def; eat IN; e1 <- p_expr; return (Let d e1)
  <+> do eat LAMBDA; xs <- p_formals; 
		e1 <- p_expr; return (nested_lam xs e1)
  <+> p_cond

-- -- {\syn _sequence_ \arrow\ _cond_ \{ ";" _cond_ \}}
p_comp =
  do es <- p_list p_cond PIPE; return (foldr1 Pipe es)
  <+> do es <- p_list p_cond PAR; return (foldr1 Par es)

-- {\syn _cond_ \arrow\ "if" _cond_ "then" _cond_ "else" _cond_}
-- {\syn\qquad\qquad\qquad \orr\ "while" _cond_ "do" _cond_ \orr\ _term7_}
p_cond = 
  do eat IF; e1 <- p_cond; eat THEN; e2 <- p_cond;
     eat ELSE; e3 <- p_cond; return (If e1 e2 e3)
  <+> p_term6

-- {\syn _term7_ \arrow\ _term6_ \{ ":=" _term6_ \}}
-- {\syn _term6_ \arrow\ _term5_ \{ ("orelse" | ">>") _term5_ \}}
-- {\syn _term5_ \arrow\ _term4_ \{ _relop_ _term4_ \}}
-- {\syn _term4_ \arrow\ _term3_ \{ _addop_ _term3_ \}}
-- {\syn _term3_ \arrow\ _term2_ \{ _mulop_ _term2_ \}}
-- {\syn _term2_ \arrow\ _term1_ \{ _consop_ _term1_ \}}
p_term6 = 
  p_chainr mk (eat ORELSE) p_term12
  where
    mk ORELSE e1 e2 = OrElse e1 e2
    mk PIPE e1 e2 = Pipe e1 e2
    mk PAR e1 e2 = Par e1 e2
p_term12 = p_chainl mk (eat PIPE <+> eat PAR) p_term5
  where 
    mk PIPE e1 e2 = Pipe e1 e2
    mk PAR e1 e2 = Par e1 e2

p_term5 = p_opchainl p_relop p_term4 
p_term4 = p_opchainl p_addop p_term3
p_term3 = p_opchainl p_mulop p_term2
p_term2 = p_opchainr (p_ident CONSOP) p_term1

p_relop = p_ident RELOP <+> (do eat EQUAL; return "=")
p_addop = p_ident ADDOP <+> (do eat MINUS; return "-")
p_mulop = p_ident MULOP <+> (do eat STAR; return "*")

p_opchainl :: Parser t Ident -> Parser t Expr -> Parser t Expr
p_opchainl p_op p_rand = 
  do e0 <- p_rand; p_tail e0
  where
    p_tail e1 =
      do w <- p_op; e2 <- p_rand; p_tail (Apply (Apply (Variable w) e1) e2)
      <+> return e1

p_opchainr :: Parser t Ident -> Parser t Expr -> Parser t Expr
p_opchainr =
  p_chainr mkop
  where mkop w e1 e2 = Apply (Apply (Variable w) e1) e2

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

-- {\syn _term1_ \arrow\ _monop_ _term1_ \orr\ "*" _term1_ \orr\ "\&" _term1_ \orr\ _term0_}
p_term1 =
  do w <- p_monop; e <- p_term1; return (Apply (Variable w) e)
  <+> do ce <- p_primary; eat RECV; return (Receive ce) 
  <+> do ce <- p_primary; eat SEND; ve <- p_primary; return (Send ce ve) 
  <+> do eat NEWCH; return NewChan 
  <+> p_term0

p_monop = p_ident MONOP <+> (do eat MINUS; return "~");

-- | primary { actuals | DOT ident } 
p_term0 =
  do e0 <- p_primary; p_qualifiers e0;
  where
    p_qualifiers e1 =
      do ap <- p_primary; p_qualifiers (Apply e1 ap)
      <+> return e1

-- {\syn _primary_ \arrow\ _number_ \orr\ _name_ \orr\ _string_ \orr\ "(" _expr_ ")"}
-- {\syn\qquad\qquad\qquad \orr\ "[" \[ _expr_ \{ "," _expr_ \} \] "]"}
-- {\syn\qquad\qquad\qquad \orr\ "\verb/{/" _base_ \[ _binding_ \{ "," _binding_ \} \] "\verb/}/"}
-- {\syn\qquad\qquad\qquad \orr\ _name_ "[" _expr_ "]"}
p_primary =
  do n <- p_number; return (Number n)
  <+> do x <- p_name; return (Variable x)
  <+> do eat LPAR; e <- p_expr; eat RPAR; return e

-- {\syn _base_ \arrow\ \[ _expr_ "\verb/|/" \]}
p_base =
  (do e <- p_expr; eat VBAR; return e)

-- {\syn _binding_ \arrow\ _name_ "=>" _expr_}
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