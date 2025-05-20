%{
#include <iostream>
#include <string>
#include <memory>
#include <vector>
#include <cstring>
#include "ast.h"
#include "SyntaxAnalyse.h"
extern "C" {
    int yylex();
    void yyerror(const char* s);
}
using namespace ast;
%}

%union {
    int int_value;
    char* string_value;
    expr_syntax* expr_ptr;
    stmt_syntax* stmt_ptr;
    var_def_stmt_syntax* var_def_ptr;
    var_decl_stmt_syntax* var_decl_ptr;
    func_def_stmt_syntax* func_def_ptr;
    func_type_syntax* func_type_ptr;
    vartype var_type;
}

// 定义token
%token <int_value> INT_CONST
%token <string_value> IDENT
%token INT VOID RETURN IF ELSE
%token PLUS MINUS MULTIPLY DIVIDE MODULO
%token ASSIGN EQUAL NON_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
%token OP_AND OP_OR OP_NOT
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA

// 定义类型
%type <expr_ptr> Exp LOrExp LAndExp EqExp RelExp AddExp MulExp UnaryExp PrimaryExp
%type <stmt_ptr> Stmt ReturnStmt BlockStmt AssignStmt VarDecl IfStmt
%type <stmt_ptr> Block Stmts
%type <var_def_ptr> VarDef
%type <var_decl_ptr> VarDefGroup
%type <func_def_ptr> FuncDef
%type <func_type_ptr> FuncType
%type <var_type> BType

// 运算符优先级（从低到高）
%left OP_OR
%left OP_AND
%left EQUAL NON_EQUAL
%left LESS LESS_EQUAL GREATER GREATER_EQUAL
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO
%right OP_NOT

%%

CompUnit: CompUnit FuncDef { SyntaxAnalyseCompUnit($$, $1, $2); }
        | CompUnit VarDecl { SyntaxAnalyseCompUnit($$, $1, $2); }
        | FuncDef { SyntaxAnalyseCompUnit($$, nullptr, $1); }
        | VarDecl { SyntaxAnalyseCompUnit($$, nullptr, $1); }
        ;

FuncDef: FuncType IDENT LPAREN RPAREN Block { SyntaxAnalyseFuncDef($$, $1, $2, nullptr, $5); }
       | FuncType IDENT LPAREN RPAREN SEMICOLON { SyntaxAnalyseFuncDef($$, $1, $2, nullptr, nullptr); }
       ;

FuncType: INT { SyntaxAnalyseFuncType($$, vartype::INT); }
        | VOID { SyntaxAnalyseFuncType($$, vartype::VOID); }
        ;

Block: LBRACE RBRACE { SyntaxAnalyseEmptyBlock($$); }
     | LBRACE Stmts RBRACE { SyntaxAnalyseBlock($$, $2); }
     ;

Stmts: Stmts Stmt { SyntaxAnalyseStmts($$, $1, $2); }
     | Stmt { SyntaxAnalyseStmts($$, nullptr, $1); }
     ;

Stmt: ReturnStmt { $$ = $1; }
    | BlockStmt { $$ = $1; }
    | AssignStmt { $$ = $1; }
    | VarDecl { $$ = $1; }
    | IfStmt { $$ = $1; }
    ;

ReturnStmt: RETURN Exp SEMICOLON { SyntaxAnalyseReturnStmt($$, $2); }
          | RETURN SEMICOLON { SyntaxAnalyseReturnStmt($$, nullptr); }
          ;

BlockStmt: Block { $$ = $1; }
         ;

AssignStmt: IDENT ASSIGN Exp SEMICOLON { SyntaxAnalyseAssignStmt($$, $1, $3); }
          ;

VarDecl: FuncType VarDefGroup SEMICOLON { SyntaxAnalyseVarDecl($$, $1, $2); }
       ;

VarDefGroup: VarDefGroup COMMA VarDef { SyntaxAnalyseVarDefGroup($$, $1, $3); }
           | VarDef { SyntaxAnalyseVarDefGroup($$, nullptr, $1); }
           ;

VarDef: IDENT { SyntaxAnalyseVarDef($$, $1, nullptr); }
      | IDENT ASSIGN Exp { SyntaxAnalyseVarDef($$, $1, $3); }
      ;

IfStmt: IF LPAREN Exp RPAREN Block { SyntaxAnalyseStmtIf($$, $3, $5, nullptr); }
      | IF LPAREN Exp RPAREN Block ELSE Block { SyntaxAnalyseStmtIf($$, $3, $5, $7); }
      ;

Exp: LOrExp { $$ = $1; }
   ;

LOrExp: LOrExp OP_OR LAndExp { SyntaxAnalyseLOrExp($$, $1, $3); }
      | LAndExp { $$ = $1; }
      ;

LAndExp: LAndExp OP_AND EqExp { SyntaxAnalyseLAndExp($$, $1, $3); }
       | EqExp { $$ = $1; }
       ;

EqExp: EqExp EQUAL RelExp { SyntaxAnalyseEqExp($$, $1, "==", $3); }
     | EqExp NON_EQUAL RelExp { SyntaxAnalyseEqExp($$, $1, "!=", $3); }
     | RelExp { $$ = $1; }
     ;

RelExp: RelExp LESS AddExp { SyntaxAnalyseRelExp($$, $1, "<", $3); }
      | RelExp LESS_EQUAL AddExp { SyntaxAnalyseRelExp($$, $1, "<=", $3); }
      | RelExp GREATER AddExp { SyntaxAnalyseRelExp($$, $1, ">", $3); }
      | RelExp GREATER_EQUAL AddExp { SyntaxAnalyseRelExp($$, $1, ">=", $3); }
      | AddExp { $$ = $1; }
      ;

AddExp: AddExp PLUS MulExp { SyntaxAnalyseAddExp($$, $1, "+", $3); }
      | AddExp MINUS MulExp { SyntaxAnalyseAddExp($$, $1, "-", $3); }
      | MulExp { $$ = $1; }
      ;

MulExp: MulExp MULTIPLY UnaryExp { SyntaxAnalyseMulExp($$, $1, "*", $3); }
      | MulExp DIVIDE UnaryExp { SyntaxAnalyseMulExp($$, $1, "/", $3); }
      | MulExp MODULO UnaryExp { SyntaxAnalyseMulExp($$, $1, "%", $3); }
      | UnaryExp { $$ = $1; }
      ;

UnaryExp: OP_NOT UnaryExp { SyntaxAnalyseUnaryExp($$, "!", $2); }
        | PLUS UnaryExp { SyntaxAnalyseUnaryExp($$, "+", $2); }
        | MINUS UnaryExp { SyntaxAnalyseUnaryExp($$, "-", $2); }
        | PrimaryExp { $$ = $1; }
        ;

PrimaryExp: INT_CONST { SyntaxAnalysePrimaryExpIntConst($$, (char*)std::to_string($1).c_str()); }
          | IDENT { SyntaxAnalysePrimaryExpVar($$, $1); }
          | LPAREN Exp RPAREN { $$ = $2; }
          ;

%%

void yyerror(const char* s) {
    std::cerr << "Error: " << s << std::endl;
}