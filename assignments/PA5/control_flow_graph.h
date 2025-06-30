#pragma once

#include <memory>
#include <string>
#include <vector>
#include "stringtab.h"

class CodeStatement {
public:
  virtual ~CodeStatement() {}
};

class FlowControlStatement : public CodeStatement {
public:
};

using CodeBlockId = int;

enum VarType {
  ObjectAttribute,
  FunctionArgument,
  Temporary
};

struct Variable {
  VarType type;
  std::string name;
};

struct ScopedVariable {
  virtual ~ScopedVariable() {};
};

struct FindMethodResult2 {
  Symbol class_name;
  int dispatch_table_index;
};

class ControlFlowGraph {
public:
  virtual Variable create_temporary() = 0;
  virtual CodeBlockId create_code_block() = 0;
  virtual std::unique_ptr<ScopedVariable> new_scoped_variable(const Symbol name, const Variable& variable) = 0;
  virtual Variable find_variable_by_name(const Symbol name) = 0;
  virtual void append_statement(CodeBlockId code_block, std::unique_ptr<CodeStatement>&& statement) = 0;
  virtual void finish_block(CodeBlockId code_block, std::unique_ptr<FlowControlStatement>&& statement) = 0;
  virtual FindMethodResult2 find_method2(const Symbol class_name, const Symbol method_name) = 0;
};
