%{
#include "parser.tab.h"
#include <cstring>
%}

%%

"int"           { return INT; }
"void"          { return VOID; }
"return"        { return RETURN; }
"if"            { return IF; }
"else"          { return ELSE; }

"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return MULTIPLY; }
"/"             { return DIVIDE; }
"%"             { return MODULO; }
"="             { return ASSIGN; }
"=="            { return EQUAL; }
"!="            { return NON_EQUAL; }
"<"             { return LESS; }
"<="            { return LESS_EQUAL; }
">"             { return GREATER; }
">="            { return GREATER_EQUAL; }
"&&"            { return OP_AND; }
"||"            { return OP_OR; }
"!"             { return OP_NOT; }

"("             { return LPAREN; }
")"             { return RPAREN; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }
";"             { return SEMICOLON; }
","             { return COMMA; }

[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.string_value = strdup(yytext);
    return IDENT;
}

[0-9]+ {
    yylval.int_value = atoi(yytext);
    return INT_CONST;
}

[ \t\n]           ;  /* 忽略空白字符 */
.                 ;  /* 忽略其他字符 */

%%