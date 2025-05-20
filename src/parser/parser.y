%{
#include <iostream>
#include <cstring>
#include "SyntaxAnalyse.hpp"
extern "C" int yylex();
extern "C" void yyerror(const char *s);
extern ast::SyntaxTree syntax_tree;
%}

%union {
    int int_value;
    char *string_value;
    ast::compunit_syntax *compunit;
    ast::func_def_syntax *func_def;
    ast::block_syntax *block;
    ast::stmt_syntax *stmt;
    ast::expr_syntax *expr;
    ast::var_def_stmt_syntax *var_def;
    ast::var_decl_stmt_syntax *var_def_group;
    ast::lval_syntax *lval;
    vartype var_type;
}

%token <int_value> INT_CONST
%token <string_value> IDENT
%token INT VOID RETURN IF ELSE
%token PLUS MINUS MULTIPLY DIVIDE MODULO ASSIGN
%token EQUAL NON_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL OP_AND OP_OR OP_NOT
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA

%type <compunit> CompUnit
%type <func_def> FuncDef
%type <block> Block BlockItems
%type <stmt> Stmt ReturnStmt BlockStmt AssignStmt VarDecl
%type <expr> PrimaryExp AddExp MulExp UnaryExp RelExp EqExp LAndExp LOrExp
%type <var_def> VarDef
%type <var_def_group> VarDefGroup
%type <lval> Lval
%type <var_type> FuncType

%%

CompUnit: CompUnit FuncDef { SyntaxAnalyseCompUnit($$, $1, $2); }
        | FuncDef { SyntaxAnalyseCompUnit($$, nullptr, $1); }
        ;

FuncDef: FuncType IDENT LPAREN RPAREN Block { SyntaxAnalyseFuncDef($$, $1, $2, $5); }
       ;

FuncType: INT { SynataxAnalyseFuncType($$, "int"); }
        | VOID { SynataxAnalyseFuncType($$, "void"); }
        ;

Block: LBRACE BlockItems RBRACE { SynataxAnalyseBlock($$, $2); }
     ;

BlockItems: BlockItems Stmt { SynataxAnalyseBlockItems($$, $1, $2); }
          | Stmt { SynataxAnalyseBlockItems($$, nullptr, $1); }
          | /* empty */ { SynataxAnalyseBlockItems($$, nullptr, nullptr); }
          ;

Stmt: ReturnStmt { $$ = $1; }
    | BlockStmt { $$ = $1; }
    | AssignStmt { $$ = $1; }
    | VarDecl { $$ = $1; }
    ;

ReturnStmt: RETURN PrimaryExp SEMICOLON { SynataxAnalyseStmtReturn($$, $2); }
          ;

BlockStmt: Block { SynataxAnalyseStmtBlock($$, $1); }
         ;

AssignStmt: Lval ASSIGN PrimaryExp SEMICOLON { SynataxAnalyseStmtAssign($$, $1, $3); }
          ;

VarDecl: VarDefGroup SEMICOLON { SynataxAnalyseVarDecl($$, nullptr, $1); }
       ;

VarDefGroup: VarDefGroup COMMA VarDef { SynataxAnalyseVarDefGroup($$, $3, $1); }
           | VarDef { SynataxAnalyseVarDefGroup($$, $1, nullptr); }
           ;

VarDef: IDENT { SynataxAnalyseVarDef($$, $1, nullptr); }
      | IDENT ASSIGN PrimaryExp { SynataxAnalyseVarDef($$, $1, $3); }
      ;

PrimaryExp: INT_CONST { SynataxAnalysePrimaryExpIntConst($$, (char*)std::to_string($1).c_str()); }
          | IDENT { SynataxAnalysePrimaryExpVar($$, $1); }
          | LPAREN AddExp RPAREN { $$ = $2; }
          ;

AddExp: AddExp PLUS MulExp { SynataxAnalyseAddExp($$, $1, "+", $3); }
      | AddExp MINUS MulExp { SynataxAnalyseAddExp($$, $1, "-", $3); }
      | MulExp { $$ = $1; }
      ;

MulExp: MulExp MULTIPLY UnaryExp { SynataxAnalyseMulExp($$, $1, "*", $3); }
      | MulExp DIVIDE UnaryExp { SynataxAnalyseMulExp($$, $1, "/", $3); }
      | MulExp MODULO UnaryExp { SynataxAnalyseMulExp($$, $1, "%", $3); }
      | UnaryExp { $$ = $1; }
      ;

UnaryExp: PLUS UnaryExp { SynataxAnalyseUnaryExp($$, "+", $2); }
        | MINUS UnaryExp { SynataxAnalyseUnaryExp($$, "-", $2); }
        | OP_NOT UnaryExp { SynataxAnalyseUnaryExp($$, "!", $2); }
        | PrimaryExp { $$ = $1; }
        ;

RelExp: RelExp EQUAL EqExp { SynataxAnalyseRelExp($$, $1, "==", $3); }
      | RelExp NON_EQUAL EqExp { SynataxAnalyseRelExp($$, $1, "!=", $3); }
      | RelExp LESS EqExp { SynataxAnalyseRelExp($$, $1, "<", $3); }
      | RelExp LESS_EQUAL EqExp { SynataxAnalyseRelExp($$, $1, "<=", $3); }
      | RelExp GREATER EqExp { SynataxAnalyseRelExp($$, $1, ">", $3); }
      | RelExp GREATER_EQUAL EqExp { SynataxAnalyseRelExp($$, $1, ">=", $3); }
      | EqExp { $$ = $1; }
      ;

EqExp: EqExp OP_AND LAndExp { SynataxAnalyseEqExp($$, $1, "&&", $3); }
     | EqExp OP_OR LAndExp { SynataxAnalyseEqExp($$, $1, "||", $3); }
     | LAndExp { $$ = $1; }
     ;

LAndExp: LAndExp OP_AND LOrExp { SynataxAnalyseLAndExp($$, $1, $3); }
       | LOrExp { $$ = $1; }
       ;

LOrExp: LOrExp OP_OR RelExp { SynataxAnalyseLOrExp($$, $1, $3); }
      | RelExp { $$ = $1; }
      ;

Lval: IDENT { SynataxAnalyseLval($$, $1); }
    ;

%%

void yyerror(const char *s) {
    std::cerr << "Syntax error: " << s << std::endl;
}