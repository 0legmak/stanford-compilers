#pragma once
#include <memory>
#include <vector>

class Expression_class;
typedef Expression_class *Expression;
class Entry;
typedef Entry* Symbol;

enum class Register : int {
    ZERO = 0,   // Zero register
    AT   = 1,
    V0   = 2,
    V1   = 3,
    A0   = 4,   // Accumulator
    ACC  = A0,
    A1   = 5,   // For arguments to prim funcs
    A2   = 6,
    A3   = 7,
    T0   = 8,
    T1   = 9,
    T2   = 10,
    T3   = 11,
    T4   = 12,
    T5   = 13,
    T6   = 14,
    T7   = 15,
    S0   = 16,  // Ptr to self (callee saves)
    SELF = S0,
    S1   = 17,  // Temporary 1
    S2   = 18,  // Temporary 2
    S3   = 19,  // Temporary 3
    S4   = 20,
    S5   = 21,
    S6   = 22,
    S7   = 23,
    T8   = 24,
    T9   = 25,
    K0   = 26,
    K1   = 27,
    GP   = 28,
    SP   = 29,  // Stack pointer
    FP   = 30,  // Frame pointer
    RA   = 31,  // Return address
};

struct SymbolLocation {
  Register reg;
  int offset;
};

struct FindMethodResult {
  Symbol class_name;
  int dispatch_table_index;
  const std::vector<Symbol>& arg_names;
};

struct Annotate {
  virtual ~Annotate() {};
};

class CodeGenerator {
public:
  virtual ~CodeGenerator() {};
  virtual int create_label() = 0;
  virtual void push(Register reg) = 0;
  virtual void pop(Register reg) = 0;
  virtual void assign(SymbolLocation loc, Expression expr) = 0;
  virtual int allocate_stack_space(int word_cnt) = 0;
  virtual void free_stack_space(int word_cnt, bool emit_code) = 0;
  virtual SymbolLocation get_symbol_location(Symbol name) = 0;
  virtual void push_symbol_location(Symbol name, SymbolLocation loc) = 0;
  virtual void pop_symbol_location() = 0;
  virtual FindMethodResult find_method(Symbol class_name, Symbol method_name) = 0;
  virtual std::vector<int> create_jump_table(const std::vector<Symbol>& types) = 0;
  virtual char* get_filename() = 0;
  virtual std::unique_ptr<Annotate> annotate(const std::string& message, int line_number) = 0;
};
