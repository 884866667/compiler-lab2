%{
#include "parser.hpp"
#include <cstring>
extern "C" int yylval;
extern "C" int yylex();
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
    yylval = strdup(yytext);
    return IDENT;
}

[0-9]+ {
    yylval = atoi(yytext);
    return INT_CONST;
}

[ \t\n] ; /* 忽略空白字符 */

. {
    // 处理非法字符
    std::cerr << "Illegal character: " << yytext << std::endl;
    return -1;
}

%%