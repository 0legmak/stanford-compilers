//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <vector>
#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include "symtab.h"
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

class TypeChecker {
public:
	virtual ~TypeChecker() {}
    virtual std::ostream& get_error_stream(tree_node* node) = 0;
	virtual SymbolTable<Symbol, Entry>& get_symbol_table() = 0;
	virtual Symbol get_current_class() = 0;
	virtual bool has_class(const Symbol class_name) = 0;
	virtual std::vector<Symbol> find_method(const Symbol class_name, const Symbol method_name) = 0;
	virtual bool is_class_conformant(const Symbol child_class, const Symbol parent_class) = 0;
	virtual Symbol lub(const std::vector<Symbol>& classes) = 0;
};

struct CaseTypes {
	Symbol declared_type;
	Symbol expr_type;
};

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0;      \
virtual Symbol get_parent() = 0;      \
virtual Features get_features() = 0; \


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                    \
Symbol get_name() override;      \
Symbol get_parent() override;      \
Features get_features() override; \


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; \
virtual bool is_method() = 0; \
virtual Symbol get_name() = 0; \
virtual Symbol get_type() = 0; \
virtual Formals get_formals() = 0; \
virtual Expression get_expr() = 0; \


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    \
bool is_method() override; \
Symbol get_name() override; \
Symbol get_type() override; \
Formals get_formals() override; \
Expression get_expr() override; \





#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0; \
virtual Symbol get_type() = 0; \


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int); \
Symbol get_name() override; \
Symbol get_type() override; \


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0; \
virtual CaseTypes check_types(TypeChecker& type_checker) = 0; \


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
CaseTypes check_types(TypeChecker& type_checker) override; \


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
virtual void check_types(TypeChecker& type_checker) = 0; \

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);  \
void check_types(TypeChecker& type_checker) override; \

#endif
