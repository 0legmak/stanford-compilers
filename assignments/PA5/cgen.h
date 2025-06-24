#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <stack>
#include <unordered_map>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class DiscardOrForwardStream : public std::ostream {
public:
   DiscardOrForwardStream(std::ostream& target)
      : std::ostream(target.rdbuf()), real_stream(target)
   {}
   void set_enabled(bool enable) {
      enabled = enable;
      rdbuf(enabled ? real_stream.rdbuf() : &nb);
   }
private:
   class NullBuf : public std::streambuf {
   protected:
      int overflow(int c) override { return traits_type::not_eof(c); }
   };

   std::ostream& real_stream;
   NullBuf nb;
   bool enabled = true;
};

class CgenClassTable : public SymbolTable<Symbol,CgenNode>, public CodeGenerator {
private:
   List<CgenNode> *nds;
   DiscardOrForwardStream str;
   int stringclasstag = -1;
   int intclasstag = -1;
   int boolclasstag = -1;
   
   struct MethodInfo {
      int dispatch_table_index;
      std::vector<Symbol> arg_names;
   };
   std::unordered_map<Symbol, std::unordered_map<Symbol, MethodInfo>> method_table;
   std::unordered_map<Symbol, std::stack<SymbolLocation>> symbol_environment;
   std::stack<Symbol> stack_symbols;
   int curr_fp_offset = 0;
   int label_id = 0;
   CgenNode* current_class_node = nullptr;
   int annotation_indent = 0;
   std::stack<SymbolLocation> temporaries_stack;
   std::vector<Register> registers_for_temporaries;
   size_t registers_used_cnt = 0;

   // stats from phase 1
   size_t temporaries_used = 0;
   void reset_stats() {
      temporaries_used = 0;
   }

   int create_label() override;
   SymbolLocation allocate_temporary() override;
   void free_temporary() override;
   SymbolLocation get_symbol_location(Symbol name) override;
   void push_symbol_location(Symbol name, SymbolLocation loc) override;
   void pop_symbol_location() override;
   FindMethodResult find_method(Symbol class_name, Symbol method_name) override;
	std::vector<int> create_jump_table(const std::vector<Symbol>& types) override;
	char* get_filename() override;
	std::unique_ptr<Annotate> annotate(const std::string& message, int line_number) override;


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
   void prepare_registers_for_temporaries();
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
  bool get_val() const;
};

