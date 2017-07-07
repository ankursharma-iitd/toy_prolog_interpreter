{
    open Lexing
    open Parser
    open Printf
    exception SyntaxError of string
}

let digit = ['0'-'9']
let constant = ['a'-'z'] (['a'-'z']|['A'-'Z']|digit|'_'|'`')*
let variable = ['A'-'Z'] (['a'-'z']|['A'-'Z']|digit|'_'|'`')*
let newline = '\t' | '\n' | "\r\n"

rule token = parse
    | [' ' '\t']        {token lexbuf}
    | newline           {NEWLINE}
    | "not"             {NOT}
    | ','               {COMMA}
    | ":-"              {IF}
    | "?-"              {GOAL}
    | '('               {LPAREN}
    | ')'               {RPAREN}
    | '='               {EQUALS}
    | '.'               {DOT}
    | '!'               {CUT}
    | '['               {LBRACE}
    | ']'               {RBRACE}
    | '\''              {APOSTROPHE}
    | ';'               {SEMICOLON}
    | constant          {CONST (lexeme lexbuf)}
    | variable          {VAR (lexeme lexbuf)}
    | _                 {raise (SyntaxError ("Unexpectec char: " ^ (lexeme lexbuf)))}
    |eof                {EOF}
    |"##"               {exit 0}