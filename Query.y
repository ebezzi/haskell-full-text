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
  '(' 	{ TokenLParen }
  '"'   { TokenQuotes }
  ')' 	{ TokenRParen }

%%

-- ciao a tutti : BooleanQuery(TermQuery(ciao), TQ(a), TQ(tutti))
-- ciao (a tutti): BooleanQuery(TQ(ciao), BooleanQuery(TQ(a) ,TQ(tutti)))
-- ciao ema: BooleanQuery(TQ(ciao), TQ(ema))
-- ciao (ema): BooleanQuery(TQ(ciao), TQ(ema))

Query   : Clause          { [$1] }
        | Query Clause    { $2 : $1 }

Clause  : Term            { $1 }
        | '(' Query ')'   { BooleanQuery $2 }
        | '"' Words '"'   { PhraseQuery $2 }

Words   : word            { [$1] }
        | Words word      { $2 : $1 }

Term    : word            { TermQuery $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Query = TermQuery String | BooleanQuery [Query] | PhraseQuery [String] deriving Show

data Token = TokenOR | TokenAND | TokenWord String | TokenLParen | TokenRParen | TokenQuotes deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('"':cs) = TokenQuotes : lexer cs
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



