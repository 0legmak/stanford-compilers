//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include <vector>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void cgen(ostream&) = 0;		\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void cgen(ostream&);     			\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_name() = 0;  	\
virtual Symbol get_parent() = 0;    	\
virtual Symbol get_filename() = 0;      \
virtual Features get_features() = 0; \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                                  \
Symbol get_name() override { return name; }		       \
Symbol get_parent() override { return parent; }     	       \
Symbol get_filename() override { return filename; }             \
Features get_features() override { return features; }  \
void dump_with_types(ostream&,int) override;                    \


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0; \
virtual bool is_method() = 0; \
virtual Symbol get_type() = 0; \
virtual Expression get_expr() = 0; \
virtual Formals get_formals() = 0; \


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    \
Symbol get_name() override { return name; }; \
bool is_method() override; \
Symbol get_type() override; \
Expression get_expr() override; \
Formals get_formals() override; \


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0; \


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int); \
Symbol get_name() override { return name; } \


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0; \
virtual Symbol get_name() = 0; \
virtual Symbol get_type() = 0; \
virtual Expression get_expr() = 0; \


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
Symbol get_name() override { return name; } \
Symbol get_type() override { return type_decl; } \
Expression get_expr() override { return expr; } \

struct SymbolLocation {
	char* reg;
	int offset;
};

struct FindMethodResult {
	Symbol class_name;
	int dispatch_table_index;
	const std::vector<Symbol>& arg_names;
};

class CodeGenerator {
public:
	virtual ~CodeGenerator() {};
	virtual int get_label() = 0;
	virtual void push(char* reg) = 0;
   	virtual void pop(char* reg) = 0;
	virtual int allocate_stack_space(int word_cnt) = 0;
	virtual void free_stack_space(int word_cnt, bool emit_code) = 0;
	virtual SymbolLocation get_symbol_location(Symbol name) = 0;
	virtual void push_symbol_location(Symbol name, SymbolLocation loc) = 0;
	virtual void pop_symbol_location() = 0;
	virtual FindMethodResult find_method(Symbol class_name, Symbol method_name) = 0;
	virtual std::vector<int> create_jump_table(const std::vector<Symbol>& types) = 0;
	virtual char* get_filename() = 0;
};

#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void code(ostream&, CodeGenerator& codegen) = 0; \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }

#define Expression_SHARED_EXTRAS           \
void code(ostream&, CodeGenerator& codegen); 			   \
void dump_with_types(ostream&,int); 


#endif
