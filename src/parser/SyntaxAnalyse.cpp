#include "SyntaxAnalyse.h"
#include <cstring>

namespace ast {

// CompUnit
void SyntaxAnalyseCompUnit(comp_unit_syntax *&self, comp_unit_syntax *prev, stmt_syntax *stmt) {
    if (!self) {
        self = new comp_unit_syntax;
    }
    if (prev) {
        self->stmts.insert(self->stmts.end(), prev->stmts.begin(), prev->stmts.end());
    }
    if (stmt) {
        self->stmts.push_back(std::shared_ptr<stmt_syntax>(stmt));
    }
}

// FuncDef
void SyntaxAnalyseFuncDef(func_def_stmt_syntax *&self, func_type_syntax *type, char *name, param_list_syntax *params, stmt_syntax *body) {
    self = new func_def_stmt_syntax;
    self->func_type = std::shared_ptr<func_type_syntax>(type);
    self->name = name;
    self->params = std::shared_ptr<param_list_syntax>(params);
    self->body = std::shared_ptr<stmt_syntax>(body);
}

// FuncType
void SyntaxAnalyseFuncType(func_type_syntax *&self, vartype type) {
    self = new func_type_syntax;
    self->type = type;
}

// Block
void SyntaxAnalyseEmptyBlock(block_stmt_syntax *&self) {
    self = new block_stmt_syntax;
}

void SyntaxAnalyseBlock(block_stmt_syntax *&self, stmt_syntax *stmts) {
    self = new block_stmt_syntax;
    if (stmts) {
        self->stmts = std::shared_ptr<stmt_syntax>(stmts);
    }
}

// Stmts
void SyntaxAnalyseStmts(stmts_syntax *&self, stmts_syntax *prev, stmt_syntax *stmt) {
    if (!self) {
        self = new stmts_syntax;
    }
    if (prev) {
        if (prev->stmts) {
            self->stmts.push_back(std::shared_ptr<stmt_syntax>(prev->stmts));
        }
    }
    if (stmt) {
        self->stmts.push_back(std::shared_ptr<stmt_syntax>(stmt));
    }
}

// ReturnStmt
void SyntaxAnalyseReturnStmt(return_stmt_syntax *&self, expr_syntax *expr) {
    self = new return_stmt_syntax;
    self->expr = std::shared_ptr<expr_syntax>(expr);
}

// AssignStmt
void SyntaxAnalyseAssignStmt(assign_stmt_syntax *&self, char *lhs, expr_syntax *rhs) {
    self = new assign_stmt_syntax;
    self->lhs = lhs;
    self->rhs = std::shared_ptr<expr_syntax>(rhs);
}

// VarDecl
void SyntaxAnalyseVarDecl(ast::stmt_syntax *&self, ast::func_type_syntax *type, ast::var_decl_stmt_syntax *var_def_group) {
    auto syntax = new ast::var_decl_stmt_syntax;
    if (var_def_group) {
        for (auto def : var_def_group->var_def_list) {
            def->restype = type->type;
            syntax->var_def_list.emplace_back(def);
        }
    }
    self = static_cast<ast::stmt_syntax*>(syntax);
}

// VarDefGroup
void SyntaxAnalyseVarDefGroup(var_decl_stmt_syntax *&self, var_decl_stmt_syntax *prev, var_def_stmt_syntax *def) {
    if (!self) {
        self = new var_decl_stmt_syntax;
    }
    if (prev) {
        self->var_def_list.insert(self->var_def_list.end(), prev->var_def_list.begin(), prev->var_def_list.end());
    }
    if (def) {
        self->var_def_list.push_back(std::shared_ptr<var_def_stmt_syntax>(def));
    }
}

// VarDef
void SyntaxAnalyseVarDef(var_def_stmt_syntax *&self, char *ident, expr_syntax *init) {
    self = new var_def_stmt_syntax;
    self->name = ident;
    self->restype = vartype::INT;
    self->initializer = std::shared_ptr<expr_syntax>(init);
}

// IfStmt
void SyntaxAnalyseStmtIf(ast::stmt_syntax *&self, ast::expr_syntax *cond, ast::stmt_syntax *then_body, ast::stmt_syntax *else_body) {
    auto syntax = new ast::if_stmt_syntax;
    syntax->pred = std::shared_ptr<ast::expr_syntax>(cond);
    syntax->then_body = std::shared_ptr<ast::stmt_syntax>(then_body);
    syntax->else_body = std::shared_ptr<ast::stmt_syntax>(else_body);
    self = static_cast<ast::stmt_syntax*>(syntax);
}

// Expressions
void SyntaxAnalyseLOrExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, ast::expr_syntax *cond2) {
    auto syntax = new ast::logic_cond_syntax;
    syntax->op = ast::relop::op_or;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(cond2);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalyseLAndExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, ast::expr_syntax *cond2) {
    auto syntax = new ast::logic_cond_syntax;
    syntax->op = ast::relop::op_and;
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(cond2);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalyseEqExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, char *op, ast::expr_syntax *cond2) {
    auto syntax = new ast::rel_cond_syntax;
    if (strcmp(op, "==") == 0) {
        syntax->op = ast::relop::equal;
    } else if (strcmp(op, "!=") == 0) {
        syntax->op = ast::relop::non_equal;
    }
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(cond2);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalyseRelExp(ast::expr_syntax *&self, ast::expr_syntax *cond1, char *op, ast::expr_syntax *cond2) {
    auto syntax = new ast::rel_cond_syntax;
    if (strcmp(op, "<") == 0) {
        syntax->op = ast::relop::less;
    } else if (strcmp(op, "<=") == 0) {
        syntax->op = ast::relop::less_equal;
    } else if (strcmp(op, ">") == 0) {
        syntax->op = ast::relop::greater;
    } else if (strcmp(op, ">=") == 0) {
        syntax->op = ast::relop::greater_equal;
    }
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(cond1);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(cond2);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalyseAddExp(ast::expr_syntax *&self, ast::expr_syntax *lhs, char *op, ast::expr_syntax *rhs) {
    auto syntax = new ast::binop_expr_syntax;
    if (strcmp(op, "+") == 0) {
        syntax->op = ast::binop::plus;
    } else if (strcmp(op, "-") == 0) {
        syntax->op = ast::binop::minus;
    }
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(lhs);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(rhs);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalyseMulExp(ast::expr_syntax *&self, ast::expr_syntax *lhs, char *op, ast::expr_syntax *rhs) {
    auto syntax = new ast::binop_expr_syntax;
    if (strcmp(op, "*") == 0) {
        syntax->op = ast::binop::multiply;
    } else if (strcmp(op, "/") == 0) {
        syntax->op = ast::binop::divide;
    } else if (strcmp(op, "%") == 0) {
        syntax->op = ast::binop::modulo;
    }
    syntax->lhs = std::shared_ptr<ast::expr_syntax>(lhs);
    syntax->rhs = std::shared_ptr<ast::expr_syntax>(rhs);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalyseUnaryExp(ast::expr_syntax *&self, char *op, ast::expr_syntax *operand) {
    auto syntax = new ast::unary_expr_syntax;
    if (strcmp(op, "!") == 0) {
        syntax->op = ast::unaryop::not_op;
    } else if (strcmp(op, "+") == 0) {
        syntax->op = ast::unaryop::plus;
    } else if (strcmp(op, "-") == 0) {
        syntax->op = ast::unaryop::minus;
    }
    syntax->operand = std::shared_ptr<ast::expr_syntax>(operand);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalysePrimaryExpIntConst(ast::expr_syntax *&self, char *value) {
    auto syntax = new ast::int_literal_syntax;
    syntax->value = atoi(value);
    self = static_cast<ast::expr_syntax*>(syntax);
}

void SyntaxAnalysePrimaryExpVar(ast::expr_syntax *&self, char *name) {
    auto syntax = new ast::var_expr_syntax;
    syntax->name = name;
    self = static_cast<ast::expr_syntax*>(syntax);
}

} // namespace ast