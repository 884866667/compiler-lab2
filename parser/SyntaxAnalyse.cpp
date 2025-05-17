#include "SyntaxAnalyse.hpp"
#include "cstring"

extern ast::SyntaxTree syntax_tree;

// 已实现的函数保持不变
void SyntaxAnalyseCompUnit(ast::compunit_syntax * &self, ast::compunit_syntax *compunit, ast::func_def_syntax *func_def)
{
    if(compunit){
        self = new ast::compunit_syntax;
        for(auto  i : compunit->global_defs){
            self->global_defs.emplace_back(std::shared_ptr<ast::func_def_syntax>(i));
        }
        self->global_defs.emplace_back(func_def);
    }else{
        self = new ast::compunit_syntax;
        self->global_defs.emplace_back(func_def);
    }
    syntax_tree.root = self;
}

void SyntaxAnalyseFuncDef(ast::func_def_syntax * &self, vartype var_type, char *Ident, ast::block_syntax *block)
{
    self = new ast::func_def_syntax;
    self->name = Ident;
    self->rettype = var_type;
    self->body = std::shared_ptr<ast::block_syntax>(block);
}

void SynataxAnalyseFuncType(vartype &self, char* type)
{
    self = ( !strcmp(type,"int") ? vartype::INT : vartype::VOID);
}

void SynataxAnalyseBlock(ast::block_syntax *&self, ast::block_syntax *block_items)
{
    self = new ast::block_syntax;
    if(block_items){
        for(auto  i : block_items->body){
            self->body.emplace_back(i);
        }
    }
}    

void SynataxAnalyseBlockItems(ast::block_syntax *&self, ast::block_syntax *block_items, ast::stmt_syntax *stmt)
{
    self = new ast::block_syntax;
    if(block_items && stmt){
        for(auto  i : block_items->body){
            self->body.emplace_back(i);
        }
        self->body.emplace_back(stmt);
    }else if(!stmt && !block_items){
        self = nullptr;
    }else {
        self->body.emplace_back(stmt);
    }
}

void SynataxAnalyseStmtReturn(ast::stmt_syntax *&self, ast::expr_syntax *exp)
{
    auto syntax = new ast::return_stmt_syntax;
    if(exp)
        syntax->exp = std::shared_ptr<ast::expr_syntax>(exp);
    self = static_cast<ast::stmt_syntax*>(syntax);
}

void SynataxAnalysePrimaryExpIntConst(ast::expr_syntax *&self, char *current_symbol)
{
    auto syntax = new ast::literal_syntax;
    syntax->intConst = std::stoi(current_symbol);
    self = static_cast<ast::expr_syntax*>(syntax);
}

// 完善 a- 难度的函数
void SynataxAnalyseStmtBlock(ast::stmt_syntax *&self, ast::block_syntax *block)
{
    self = new ast::block_stmt_syntax;
    self->block = std::shared_ptr<ast::block_syntax>(block);
}

void SynataxAnalysePrimaryExpVar(ast::expr_syntax* &self, char* current_symbol)
{
    auto syntax = new ast::lval_syntax;
    syntax->name = current_symbol;
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SynataxAnalyseVarDecl(ast::stmt_syntax *&self, ast::var_def_stmt_syntax *var_def, ast::var_decl_stmt_syntax *var_def_group)
{
    self = new ast::var_decl_stmt_syntax;
    if (var_def_group) {
        for (auto def : var_def_group->var_def_list) {
            self->var_def_list.emplace_back(def);
        }
    }
    self->var_def_list.emplace_back(std::shared_ptr<ast::var_def_stmt_syntax>(var_def));
}

void SynataxAnalyseVarDefGroup(ast::var_decl_stmt_syntax *&self, ast::var_def_stmt_syntax *var_def, ast::var_decl_stmt_syntax *var_def_group)
{
    self = new ast::var_decl_stmt_syntax;
    if (var_def_group) {
        for (auto def : var_def_group->var_def_list) {
            self->var_def_list.emplace_back(def);
        }
    }
    self->var_def_list.emplace_back(std::shared_ptr<ast::var_def_stmt_syntax>(var_def));
}

void SynataxAnalyseVarDef(ast::var_def_stmt_syntax *&self, char *ident, ast::expr_syntax *init)
{
    self = new ast::var_def_stmt_syntax;
    self->name = ident;
    if (init) {
        self->initializer = std::shared_ptr<ast::expr_syntax>(init);
    }
}

void SynataxAnalyseAddExp(ast::expr_syntax *&self, ast::expr_syntax *exp1, char *op, ast::expr_syntax *exp2)
{
    auto syntax = new ast::binop_expr_syntax;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(exp1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(exp2);
    if (!strcmp(op, "+")) {
        syntax->op = ast::binop_expr_syntax::Op::ADD;
    } else if (!strcmp(op, "-")) {
        syntax->op = ast::binop_expr_syntax::Op::SUB;
    }
    self = static_cast<ast::expr_syntax*>(syntax);
}

// 完善 a 难度的函数
void SynataxAnalyseMulExp(ast::expr_syntax *&self, ast::expr_syntax *exp1, char *op, ast::expr_syntax *exp2)
{
    auto syntax = new ast::binop_expr_syntax;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(exp1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(exp2);
    if (!strcmp(op, "*")) {
        syntax->op = ast::binop_expr_syntax::Op::MUL;
    } else if (!strcmp(op, "/")) {
        syntax->op = ast::binop_expr_syntax::Op::DIV;
    }
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SynataxAnalyseStmtAssign(ast::stmt_syntax *&self, ast::lval_syntax *target, ast::expr_syntax *value)
{
    auto syntax = new ast::assign_stmt_syntax;
    syntax->target = std::shared_ptr<ast::lval_syntax>(target);
    syntax->value = std::shared_ptr<ast::expr_syntax>(value);
    self = static_cast<ast::stmt_syntax*>(syntax);
}

void SynataxAnalyseLval(ast::lval_syntax *&self, char *ident)
{
    self = new ast::lval_syntax;
    self->name = ident;
}

// 完善 a+ 难度的函数
void SynataxAnalyseStmtIf(ast::stmt_syntax *&self, ast::expr_syntax *cond, ast::stmt_syntax *then_body, ast::stmt_syntax *else_body)
{
    auto syntax = new ast::if_stmt_syntax;
    syntax->pred = std::shared_ptr<ast::expr_syntax>(cond);
    syntax->then_body = std::shared_ptr<ast::stmt_syntax>(then_body);
    if (else_body) {
        syntax->else_body = std::shared_ptr<ast::stmt_syntax>(else_body);
    }
    self = static_cast<ast::stmt_syntax*>(syntax);
}

void SynataxAnalyseLOrExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, ast::expr_syntax *cond2)
{
    auto syntax = new ast::logic_cond_syntax;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(cond2);
    syntax->op = ast::logic_cond_syntax::Op::LOR;
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SynataxAnalyseLAndExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, ast::expr_syntax *cond2)
{
    auto syntax = new ast::logic_cond_syntax;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(cond2);
    syntax->op = ast::logic_cond_syntax::Op::LAND;
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SynataxAnalyseEqExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, char *op, ast::expr_syntax *cond2)
{
    auto syntax = new ast::logic_cond_syntax;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(cond2);
    if (!strcmp(op, "==")) {
        syntax->op = ast::logic_cond_syntax::Op::EQ;
    } else if (!strcmp(op, "!=")) {
        syntax->op = ast::logic_cond_syntax::Op::NEQ;
    }
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SynataxAnalyseRelExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, char *op, ast::expr_syntax *exp)
{
    auto syntax = new ast::rel_cond_syntax;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(exp);
    if (!strcmp(op, "<")) {
        syntax->op = ast::rel_cond_syntax::Op::LT;
    } else if (!strcmp(op, "<=")) {
        syntax->op = ast::rel_cond_syntax::Op::LEQ;
    } else if (!strcmp(op, ">")) {
        syntax->op = ast::rel_cond_syntax::Op::GT;
    } else if (!strcmp(op, ">=")) {
        syntax->op = ast::rel_cond_syntax::Op::GEQ;
    }
    self = static_cast<ast::expr_syntax*>(syntax);
}

// 完善 a++ 难度的函数
void SynataxAnalyseUnaryExp(ast::expr_syntax *&self, char *op, ast::expr_syntax *exp)
{
    auto syntax = new ast::unaryop_expr_syntax;
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(exp);
    if (!strcmp(op, "+")) {
        syntax->op = ast::unaryop_expr_syntax::Op::UPLUS;
    } else if (!strcmp(op, "-")) {
        syntax->op = ast::unaryop_expr_syntax::Op::UMINUS;
    } else if (!strcmp(op, "!")) {
        syntax->op = ast::unaryop_expr_syntax::Op::NOT;
    }
    self = static_cast<ast::expr_syntax*>(syntax);
}