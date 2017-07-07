%{
open Lexing
open Types
%}

%token GOAL
%token IF
%token LPAREN
%token RPAREN
%token COMMA
%token DOT
%token <string> CONST
%token <string> VAR
%token NOT
%token EQUALS
%token CUT EOF
%token LBRACE RBRACE
%token APOSTROPHE NEWLINE SEMICOLON

%start program
%start query
%type <Types.term list>query
%type <Types.term list list> program

%%

query:
| NEWLINE query											{$2}
| atomic_formula_list DOT NEWLINE						{$1}
;

program: /*empty */										{[]}
| program NEWLINE										{$1}							
| program clause	 		      						{$2 :: $1}
;

clause:
| atomic_formula DOT         			     				{[$1]}
| atomic_formula IF atomic_formula_list DOT                 {$1::$3}
;

atomic_formula_list:
| atomic_formula              							{[$1]}
| atomic_formula COMMA atomic_formula_list  			{$1 :: $3}
;

atomic_formula:
| CONST LPAREN term_list RPAREN       					{At($1,$3)}
;

term_list:
| term                 				 					{[$1]}
| term COMMA term_list             			   			 {$1 :: $3}
;

term:
| VAR               							 {V($1)}
| CONST               							 {Const($1)}
| atomic_formula              				     {$1}
;

%%



