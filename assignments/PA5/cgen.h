#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <unordered_map>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode>, public CodeGenerator {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag = -1;
   int intclasstag = -1;
   int boolclasstag = -1;
   StringEntry* empty_string = nullptr;
   IntEntry* zero_int = nullptr;
   
   std::unordered_map<Symbol, SymbolLocation> symbol_environment;
   int curr_fp_offset = 0;
   int label_id = 0;
   SymbolLocation get_symbol_location(Symbol name) override;
   int get_label() override;
   void push(char* reg) override;
   void pop(char* reg) override;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_dispatch_table_and_prototype_objects();
   void code_class_name_and_object_tables();
   void code_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void assign_class_tags();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int class_tag;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   void set_class_tag(int tag) { class_tag = tag; }
   int get_class_tag() const { return class_tag; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

