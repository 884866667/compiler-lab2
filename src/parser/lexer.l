%{
#include "SyntaxTree.hpp"
#include <iostream>
extern int yylineno;
extern char* yytext;
%}

%%

"int"           { return INT; }
"void"          { return VOID; }
"if"            { return IF; }
"return"        { return RETURN; }
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return MULTIPLY; }
"/"             { return DIVIDE; }
"%"             { return MODULO; }
"=="            { return EQUAL; }
"!="            { return NON_EQUAL; }
"<"             { return LESS; }
"<="            { return LESS_EQUAL; }
">"             { return GREATER; }
">="            { return GREATER_EQUAL; }
"&&"            { return AND; }
"||"            { return OR; }
"="             { return ASSIGN; }
";"             { return SEMICOLON; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }
[a-zA-Z_][a-zA-Z0-9_]* { return IDENTIFIER; }
[0-9]+          { return INTEGER; }
[ \t\n]         ; /* 忽略空白字符 */
.               { std::cerr << "Unexpected character: " << yytext << " at line " << yylineno << std::endl; }

%%

int yywrap() {
    return 1;
}
