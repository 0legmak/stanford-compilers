#include "control_flow_graph.h"
#include "cool-tree.h"

class Assign : public CodeStatement {
public:
  Assign(const Variable& result, const Variable& val) {}
};

class AssignInt : public CodeStatement {
public:
  AssignInt(const Variable& result, IntEntryP val) {}
};

class AssignString : public CodeStatement {
public:
  AssignString(const Variable& result, StringEntryP val) {}
};

class AssignBool : public CodeStatement {
public:
  AssignBool(const Variable& result, const BoolConst& val) {}
};

class AssignVoid : public CodeStatement {
public:
  AssignVoid(const Variable& result) {}
};

class Plus : public CodeStatement {
public:
  Plus(const Variable& result, const Variable& left, const Variable& right) {}
};

class Sub : public CodeStatement {
public:
  Sub(const Variable& result, const Variable& left, const Variable& right) {}
};

class Mul : public CodeStatement {
public:
  Mul(const Variable& result, const Variable& left, const Variable& right) {}
};

class Div : public CodeStatement {
public:
  Div(const Variable& result, const Variable& left, const Variable& right) {}
};

class Eq : public CodeStatement {
public:
  Eq(const Variable& result, const Variable& left, const Variable& right) {}
};

class Less : public CodeStatement {
public:
  Less(const Variable& result, const Variable& left, const Variable& right) {}
};

class LessOrEq : public CodeStatement {
public:
  LessOrEq(const Variable& result, const Variable& left, const Variable& right) {}
};

class Neg : public CodeStatement {
public:
  Neg(const Variable& result, const Variable& val) {}
};

class Not : public CodeStatement {
public:
  Not(const Variable& result, const Variable& val) {}
};

class IsVoid : public CodeStatement {
public:
  IsVoid(const Variable& result, const Variable& val) {}
};

class NewObject : public CodeStatement {
public:
  NewObject(const Variable& result, const Symbol class_name) {}
};

class PushArg : public CodeStatement {
public:
  PushArg(const Variable& var) {}
};

class DynamicDispatch : public CodeStatement {
public:
  DynamicDispatch(const Variable& result, const Variable& object, int disp_table_index) {}
};

class StaticDispatch : public CodeStatement {
public:
  StaticDispatch(const Variable& result, const Variable& object, const Symbol class_name, const Symbol method_name) {}
};

class Jump : public FlowControlStatement {
public:
  Jump(CodeBlockId next_block) {}
};

class CondJump : public FlowControlStatement {
public:
  CondJump(const Variable& var, CodeBlockId true_block, CodeBlockId false_block) {}
};

class CaseJump : public FlowControlStatement {
public:
  CaseJump(const Variable& var, const std::vector<Symbol>& types, const std::vector<CodeBlockId>& case_blocks) {}
};

CodeBlockId assign_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  auto expr_last_block = expr->compute_control_flow_graph(control_flow_graph, code_block, result);
  control_flow_graph.append_statement(expr_last_block, std::make_unique<Assign>(control_flow_graph.find_variable_by_name(name), result));
  return expr_last_block;
}

static CodeBlockId compute_dispatch(
  bool dynamic,
  Expression expr,
  Symbol type,
  Symbol method_name,
  Expressions args,
  ControlFlowGraph& control_flow_graph,
  CodeBlockId code_block,
  const Variable& result
) {
  const auto [class_name, disp_table_index] =
    control_flow_graph.find_method2(dynamic ? expr->get_type() : type, method_name);
  for (int i = args->first(); args->more(i); i = args->next(i)) {
    const auto arg_expr = args->nth(i);
    const auto arg_expr_result = control_flow_graph.create_temporary();
    code_block = arg_expr->compute_control_flow_graph(control_flow_graph, code_block, arg_expr_result);
    control_flow_graph.append_statement(code_block, std::make_unique<PushArg>(arg_expr_result));
  }
  const auto expr_result = control_flow_graph.create_temporary();
  code_block = expr->compute_control_flow_graph(control_flow_graph, code_block, expr_result);
  if (dynamic) {
    control_flow_graph.append_statement(code_block, std::make_unique<DynamicDispatch>(result, expr_result, disp_table_index));
  } else {
    control_flow_graph.append_statement(code_block, std::make_unique<StaticDispatch>(result, expr_result, class_name, method_name));
  }
  return code_block;
}

CodeBlockId static_dispatch_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_dispatch(false, expr, type_name, name, actual, control_flow_graph, code_block, result);
}

CodeBlockId dispatch_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_dispatch(true, expr, nullptr, name, actual, control_flow_graph, code_block, result);
}

CodeBlockId cond_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  const auto pred_result = control_flow_graph.create_temporary();
  const auto pred_last_block = pred->compute_control_flow_graph(control_flow_graph, code_block, pred_result);
  const auto then_first_block = control_flow_graph.create_code_block();
  const auto then_last_block = then_exp->compute_control_flow_graph(control_flow_graph, then_first_block, result);
  const auto else_first_block = control_flow_graph.create_code_block();
  const auto else_last_block = else_exp->compute_control_flow_graph(control_flow_graph, else_first_block, result);
  control_flow_graph.finish_block(pred_last_block, std::make_unique<CondJump>(pred_result, then_first_block, else_first_block));
  auto next_block = control_flow_graph.create_code_block();
  control_flow_graph.finish_block(then_last_block, std::make_unique<Jump>(next_block));
  control_flow_graph.finish_block(else_last_block, std::make_unique<Jump>(next_block));
  return next_block;
}

CodeBlockId loop_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  const auto pred_first_block = control_flow_graph.create_code_block();
  control_flow_graph.finish_block(code_block, std::make_unique<Jump>(pred_first_block));
  const auto pred_result = control_flow_graph.create_temporary();
  const auto pred_last_block = pred->compute_control_flow_graph(control_flow_graph, pred_first_block, pred_result);
  const auto body_first_block = control_flow_graph.create_code_block();
  const auto body_last_block = body->compute_control_flow_graph(control_flow_graph, body_first_block, control_flow_graph.create_temporary());
  control_flow_graph.finish_block(body_last_block, std::make_unique<Jump>(pred_first_block));
  auto next_block = control_flow_graph.create_code_block();
  control_flow_graph.finish_block(pred_last_block, std::make_unique<CondJump>(pred_result, body_first_block, next_block));
  control_flow_graph.append_statement(next_block, std::make_unique<AssignVoid>(result));
  return next_block;
}

CodeBlockId typcase_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  const auto case_expr_result = control_flow_graph.create_temporary();
  const auto expr_last_block = expr->compute_control_flow_graph(control_flow_graph, code_block, case_expr_result);
  const int cases_count = cases->len();
  std::vector<Symbol> types(cases_count);
  std::vector<CodeBlockId> case_blocks(cases_count);
  auto next_block = control_flow_graph.create_code_block();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    const auto cas = cases->nth(i);
    types[i] = cas->get_type();
    const auto case_first_block = control_flow_graph.create_code_block();
    case_blocks[i] = case_first_block;
    const auto scoped_case_var = control_flow_graph.new_scoped_variable(cas->get_name(), case_expr_result);
    const auto case_last_block = cas->get_expr()->compute_control_flow_graph(control_flow_graph, case_first_block, result);
    control_flow_graph.finish_block(case_last_block, std::make_unique<Jump>(next_block));
  }
  control_flow_graph.finish_block(expr_last_block, std::make_unique<CaseJump>(case_expr_result, types, case_blocks));
  return next_block;
}

CodeBlockId block_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  const int expr_cnt = body->len();
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    const auto expr = body->nth(i);
    code_block = expr->compute_control_flow_graph(control_flow_graph, code_block, i + 1 == expr_cnt ? result : control_flow_graph.create_temporary());
  }
  return code_block;
}

CodeBlockId let_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  const auto let_var = control_flow_graph.create_temporary();
  if (init->get_type() && init->get_type() != No_type) {
    code_block = init->compute_control_flow_graph(control_flow_graph, code_block, let_var);
  } else {
    if (type_decl == Int) {
      control_flow_graph.append_statement(code_block, std::make_unique<AssignInt>(let_var, zero_int));
    } else if (type_decl == Bool) {
      control_flow_graph.append_statement(code_block, std::make_unique<AssignBool>(let_var, falsebool));
    } else if (type_decl == Str) {
      control_flow_graph.append_statement(code_block, std::make_unique<AssignString>(let_var, empty_string));
    } else {
      control_flow_graph.append_statement(code_block, std::make_unique<AssignVoid>(let_var));
    }
  }
  const auto scoped_let_var = control_flow_graph.new_scoped_variable(identifier, let_var);
  return body->compute_control_flow_graph(control_flow_graph, code_block, result);
}

template <typename BynaryOp>
CodeBlockId compute_binary_op(Expression e1, Expression e2, ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  const auto left = control_flow_graph.create_temporary();
  const auto right = control_flow_graph.create_temporary();
  code_block = e1->compute_control_flow_graph(control_flow_graph, code_block, left);
  code_block = e2->compute_control_flow_graph(control_flow_graph, code_block, right);
  control_flow_graph.append_statement(code_block, std::make_unique<BynaryOp>(result, left, right));
  return code_block;
}

template <typename UnaryOp>
CodeBlockId compute_unary_op(Expression e, ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  const auto e_val = control_flow_graph.create_temporary();
  code_block = e->compute_control_flow_graph(control_flow_graph, code_block, e_val);
  control_flow_graph.append_statement(code_block, std::make_unique<UnaryOp>(result, e_val));
  return code_block;
}

CodeBlockId plus_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_binary_op<Plus>(e1, e2, control_flow_graph, code_block, result);
}

CodeBlockId sub_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_binary_op<Sub>(e1, e2, control_flow_graph, code_block, result);
}

CodeBlockId mul_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_binary_op<Mul>(e1, e2, control_flow_graph, code_block, result);
}

CodeBlockId divide_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_binary_op<Div>(e1, e2, control_flow_graph, code_block, result);
}

CodeBlockId neg_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_unary_op<Neg>(e1, control_flow_graph, code_block, result);
}

CodeBlockId eq_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_binary_op<Eq>(e1, e2, control_flow_graph, code_block, result);
}

CodeBlockId lt_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_binary_op<Less>(e1, e2, control_flow_graph, code_block, result);
}

CodeBlockId leq_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_binary_op<LessOrEq>(e1, e2, control_flow_graph, code_block, result);
}

CodeBlockId comp_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_unary_op<Not>(e1, control_flow_graph, code_block, result);
}

CodeBlockId int_const_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  control_flow_graph.append_statement(code_block, std::make_unique<AssignInt>(result, inttable.lookup_string(token->get_string())));
  return code_block;
}

CodeBlockId string_const_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  control_flow_graph.append_statement(code_block, std::make_unique<AssignString>(result, stringtable.lookup_string(token->get_string())));
  return code_block;
}

CodeBlockId bool_const_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  control_flow_graph.append_statement(code_block, std::make_unique<AssignBool>(result, BoolConst(val)));
  return code_block;
}

CodeBlockId new__class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  control_flow_graph.append_statement(code_block, std::make_unique<NewObject>(result, type_name));
  return code_block;
}

CodeBlockId isvoid_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  return compute_unary_op<IsVoid>(e1, control_flow_graph, code_block, result);
}

CodeBlockId no_expr_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  throw std::logic_error("no_expr must not participate in code generation");
}

CodeBlockId object_class::compute_control_flow_graph(ControlFlowGraph& control_flow_graph, CodeBlockId code_block, const Variable& result) {
  control_flow_graph.append_statement(code_block, std::make_unique<Assign>(result, control_flow_graph.find_variable_by_name(name)));
  return code_block;
}
