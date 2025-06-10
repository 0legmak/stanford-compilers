

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

Symbol class__class::get_name() {
    return name;
}

Symbol class__class::get_parent() {
    return parent;
}

Features class__class::get_features() {
    return features;
}

bool attr_class::is_method() {
    return false;
}

Symbol attr_class::get_name() {
    return name;
}

Symbol attr_class::get_type() {
    return type_decl;
}

Formals attr_class::get_formals() {
    return nil_Formals();
}

bool method_class::is_method() {
    return true;
}

Symbol method_class::get_name() {
    return name;
}

Symbol method_class::get_type() {
    return return_type;
}

Formals method_class::get_formals() {
    return formals;
}

Symbol formal_class::get_name() {
    return name;
}

Symbol formal_class::get_type() {
    return type_decl;
}


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
public:
    ClassTable(Classes);
    int errors() { return semant_errors; }
    ostream& semant_error();
    ostream& semant_error(Class_ c);
    ostream& semant_error(Symbol filename, tree_node *t);

private:
    static constexpr int kNoParent = -1;

    using MethodSignature = std::vector<Symbol>;
    struct FindMethodResult {
        int class_id;
        const MethodSignature& signature;
    };

    void install_basic_classes();
    void add_class(const Class_ cls) {
        const auto name = cls->get_name();
        if (!class_map.insert({ name, cls }).second) {
            semant_error(cls) << "Class " << name << " is already defined." << std::endl;
        }
    }
    void build_class_map(const Classes classes);
    static bool is_non_inheritable_class(const Symbol name) {
        return name == Int || name == Bool || name == Str;
    }
    int class_count() const {
        return class_map.size();
    }
    void build_inheritance_graph();
    void fill_parent(int u, int p) {
        for (const int v : inheritance_graph[u]) {
            if (v != p) {
                parent[v] = u;
                fill_parent(v, u);
            }
        }
    };
    void fill_parent() {
        parent.resize(class_count(), kNoParent);
        fill_parent(class_symbol_to_id[Object], kNoParent);
    }
    void collect_methods(int u, int p);
    std::optional<FindMethodResult> find_method(int class_id, const Symbol method_name) {
        for (; class_id != kNoParent; class_id = parent[class_id]) {
            if (const auto iter = class_methods[class_id].find(method_name); iter != class_methods[class_id].end()) {
                return std::make_optional<FindMethodResult>(class_id, iter->second);
            }
        }
        return {};
    };
    void collect_methods();

    int semant_errors;
    ostream& error_stream;
    std::unordered_map<Symbol, Class_> class_map;
    std::unordered_map<Symbol, int> class_symbol_to_id;
    std::vector<Symbol> class_id_to_symbol;
    std::vector<std::vector<int>> inheritance_graph;
    std::vector<int> parent;
    std::vector<std::unordered_map<Symbol, MethodSignature>> class_methods;
};

void ClassTable::build_class_map(const Classes classes) {
    install_basic_classes();
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        add_class(classes->nth(i));
    }
    if (!class_map.contains(Main)) {
        semant_error() << "Main class not found in class list." << std::endl;
    }
    class_id_to_symbol.resize(class_count());
    int class_name_id = 0;
    for (const auto& [name, cls] : class_map) {
        class_symbol_to_id[name] = class_name_id;
        class_id_to_symbol[class_name_id] = name;
        ++class_name_id;
    }
}

void ClassTable::build_inheritance_graph() {
    inheritance_graph.resize(class_count());
    for (const auto& [name, cls] : class_map) {
        if (name == Object) {
            continue;
        }
        const auto parent_name = cls->get_parent();
        if (!class_map.contains(parent_name)) {
            semant_error(cls) << "Parent class " << parent_name << " is not defined." << std::endl;
            continue;
        }
        if (is_non_inheritable_class(parent_name)) {
            semant_error(cls) << "Class " << parent_name << " is non-inheritable and cannot be a parent class." << std::endl;
            continue;
        }
        inheritance_graph[class_symbol_to_id[parent_name]].push_back(class_symbol_to_id[name]);
    }
    std::vector<bool> visited(class_count());
    std::vector<bool> on_stack(class_count());
    std::vector<int> cycle;
    std::optional<int> cycle_start;
    auto cycle_check = [&](auto&& cycle_check, int u) -> bool {
        if (on_stack[u]) {
            cycle_start = u;
            return false;
        }
        if (visited[u]) {
            return true;
        }
        visited[u] = true;
        on_stack[u] = true;
        for (const int v : inheritance_graph[u]) {
            if (!cycle_check(cycle_check, v)) {
                break;
            }
        }
        on_stack[u] = false;
        if (cycle_start) {
            cycle.push_back(u);
            if (u == cycle_start) {
                cycle_start.reset();
            }
        }
        return cycle.empty();
    };
    for (int u = 0; u < class_count(); ++u) {
        if (!visited[u]) {
            if (!cycle_check(cycle_check, u)) {
                auto& err_stream = semant_error(class_map[class_id_to_symbol[cycle.front()]]);
                err_stream << "Inheritance graph contains a cycle: ";
                for (const int u : cycle) {
                    err_stream << class_id_to_symbol[u] << " inherits ";
                }
                err_stream << class_id_to_symbol[cycle.front()] << "." << std::endl;
                break;
            }
        }
    }
}

void ClassTable::collect_methods(int u, int p) {
    const auto class_symbol = class_id_to_symbol[u];
    const auto cls = class_map[class_symbol];
    const auto features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        const auto feature = features->nth(i);
        if (!feature->is_method()) {
            continue;
        }
        const auto method_name = feature->get_name();
        if (class_methods[u].contains(method_name)) {
            semant_error(cls->get_filename(), feature)
                << "Class " << class_symbol << " has multiple definitions of method " << method_name << "." << std::endl;
            continue;
        }
        const auto method_signature = [&]() -> MethodSignature {
            MethodSignature method_signature;
            const auto formals = feature->get_formals();
            std::unordered_set<Symbol> formal_names;
            for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
                const auto formal = formals->nth(j);
                const auto formal_name = formal->get_name();
                const auto formal_type = formal->get_type();
                if (formal_name == self) {
                    semant_error(cls->get_filename(), formal)
                        << "Class " << class_symbol << " cannot use self as a formal parameter name in method "
                        << method_name << "." << std::endl;
                    return {};
                }
                if (!formal_names.insert(formal_name).second) {
                    semant_error(cls->get_filename(), formal)
                        << "Class " << class_symbol << " has multiple definitions of formal parameter " << formal_name
                        << " in method " << method_name << "." << std::endl;
                    return {};
                }
                if (formal_type == SELF_TYPE) {
                    semant_error(cls->get_filename(), formal)
                        << "Class " << class_symbol << " cannot use SELF_TYPE as a formal parameter type in method "
                        << method_name << "." << std::endl;
                    return {};
                }
                method_signature.push_back(formal_type);
            }
            method_signature.push_back(feature->get_type());
            return method_signature;
        }();
        if (method_signature.empty()) {
            continue;
        }
        ;
        if (const auto res = find_method(p, method_name); res && res->signature != method_signature) {
            semant_error(cls->get_filename(), feature)
                << "Class " << class_symbol << " redefines method " << method_name
                << " with a different signature than in parent class "
                << class_id_to_symbol[res->class_id] << "." << std::endl;
            continue;
        }
        class_methods[u][method_name] = method_signature;
    }
    for (const int v : inheritance_graph[u]) {
        if (v != p) {
            collect_methods(v, u);
        }
    }
};

void ClassTable::collect_methods() {
    class_methods.resize(class_count());
    collect_methods(class_symbol_to_id[Object], kNoParent);
    if (class_map.contains(Main) && !find_method(class_symbol_to_id[Main], main_meth)) {
        semant_error(class_map[Main]) << "Class " << Main << " does not define method " << main_meth << "." << std::endl;
    }
    if (semant_debug) {
        for (int u = 0; u < class_count(); ++u) {
            const auto& methods = class_methods[u];
            std::cerr << "Class " << class_id_to_symbol[u] << " methods:\n";
            for (const auto& [method_name, signature] : methods) {
                std::cerr << "  " << method_name << "(";
                for (size_t i = 0; i + 1 < signature.size(); ++i) {
                    std::cerr << signature[i];
                    if (i + 2 < signature.size()) std::cerr << ", ";
                }
                std::cerr << ") : " << signature.back() << "\n";
            }
        }
    }
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    build_class_map(classes);
    build_inheritance_graph();
    fill_parent();
    collect_methods();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    class_map[Object] = Object_class;
    class_map[IO] = IO_class;
    class_map[Int] = Int_class;
    class_map[Bool] = Bool_class;
    class_map[Str] = Str_class;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


