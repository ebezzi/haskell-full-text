{
module Query where
import Data.List
import Data.Char
}

%name query
%tokentype { Token }
%error { parseError }

%token
	OR		{ TokenOR }
	AND 	{ TokenAND }
	word	{ TokenWord $$ }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '(' 	{ TokenLParen }
  '"'   { TokenQuotes }
  ')' 	{ TokenRParen }

%%

Query   : Clause          { [$1] }
        | Query Clause    { $2 : $1 }

Clause  : '+' Pred        { Clause And $2 }
        | '-' Pred        { Clause Not $2 }
        | Pred            { Clause Or $1 }

Pred    : Term            { $1 }
        | '(' Query ')'   { BooleanQuery $2 }
        | '"' Words '"'   { PhraseQuery $2 }

Words   : word            { [$1] }
        | Words word      { $2 : $1 }

Term    : word            { TermQuery $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Op = And | Or | Not deriving Show
data Clause = Clause Op Query deriving Show
data Query = TermQuery String | BooleanQuery [Clause] | PhraseQuery [String] deriving Show

data Token = TokenOR | TokenAND | TokenWord String | TokenPlus | TokenMinus | TokenLParen | TokenRParen | TokenQuotes deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('"':cs) = TokenQuotes : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs

lexVar cs =
   case span isAlpha cs of
      (var,rest)   -> TokenWord var : lexer rest

-- lexer :: String -> [Token]
-- query -> [Token] -> [Query]

--main = getContents >>= print . query . lexer
parse = BooleanQuery . query . lexer
}



