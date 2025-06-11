

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <bit>
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

Expression attr_class::get_expr() {
    return init;
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

Expression method_class::get_expr() {
    return expr;
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

class ClassTable : public TypeChecker {
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
    struct FindAttributeResult {
        int class_id;
        const Symbol type;
    };
    struct LcaSparseTableNode {
        int depth;
        int node;
        bool operator<(const LcaSparseTableNode& other) const {
            return depth < other.depth;
        }
    };

    void install_basic_classes();
    void add_class(const Class_ cls) {
        const auto name = cls->get_name();
        if (!class_map.insert({ name, cls }).second) {
            semant_error(cls) << "Class " << name << " is already defined." << std::endl;
        }
    }
    void build_class_map(const Classes classes);
    static bool is_builtin_class(const Symbol name) {
        return name == Object || name == Int || name == Bool || name == Str || name == IO;
    }
    static bool is_non_inheritable_class(const Symbol name) {
        return name == Int || name == Bool || name == Str;
    }
    int class_count() const {
        return class_map.size();
    }
    void build_inheritance_graph();
    void precompute_lca_lookup_table();
    int lookup_lca(const std::vector<int>& nodes);
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
    void collect_attributes(int u, int p);
    std::optional<FindAttributeResult> find_attribute(int class_id, const Symbol attr_name) {
        for (; class_id != kNoParent; class_id = parent[class_id]) {
            if (const auto iter = class_attributes[class_id].find(attr_name); iter != class_attributes[class_id].end()) {
                return std::make_optional<FindAttributeResult>(class_id, iter->second);
            }
        }
        return {};
    };
    void collect_attributes();

    // TypeCheckerItf
    std::ostream& get_error_stream(tree_node* node) override {
        return semant_error(current_class->get_filename(), node);
    }
	SymbolTable<Symbol, Entry>& get_symbol_table() override {
        return symbol_table;
    }
 	Symbol find_class(const Symbol class_name) override {
        const auto iter = class_map.find(class_name);
        return iter != class_map.end() ? iter->second->get_name() : nullptr;
    }
 	std::vector<Symbol> find_method(const Symbol class_name, const Symbol method_name) override {
        const auto class_id = class_symbol_to_id.at(class_name);
        if (const auto res = find_method(class_id, method_name)) {
            return res->signature;
        }
        return {};
    }
	bool is_class_conformant(const Symbol child_class, const Symbol parent_class) override {
        const auto parent_class_id = class_symbol_to_id.at(parent_class);
        return lookup_lca({parent_class_id, class_symbol_to_id.at(child_class)}) == parent_class_id;
    }
    Symbol lub(const std::vector<Symbol>& classes) override {
        std::vector<int> nodes(classes.size());
        for (size_t i = 0; i < classes.size(); ++i) {
            nodes[i] = class_symbol_to_id.at(classes[i]);
        }
        return class_id_to_symbol[lookup_lca(nodes)];
    }

    void traverse_class_hierarchy(int class_id, int parent_id);
    void check_types() {
        traverse_class_hierarchy(class_symbol_to_id.at(Object), kNoParent);
    }

    int semant_errors;
    ostream& error_stream;
    std::unordered_map<Symbol, Class_> class_map;
    std::unordered_map<Symbol, int> class_symbol_to_id;
    std::vector<Symbol> class_id_to_symbol;
    std::vector<std::vector<int>> inheritance_graph;
    std::vector<int> parent;
    std::vector<std::vector<LcaSparseTableNode>> lca_sparse_table;
    std::vector<int> lca_last_occurence;
    std::vector<std::unordered_map<Symbol, MethodSignature>> class_methods;
    std::vector<std::unordered_map<Symbol, Symbol>> class_attributes;
    Class_ current_class;
    SymbolTable<Symbol, Entry> symbol_table;
};

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    build_class_map(classes);
    build_inheritance_graph();
    if (semant_errors) {
        return;
    }
    precompute_lca_lookup_table();
    collect_methods();
    collect_attributes();
    check_types();
}

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
    parent.resize(class_count(), kNoParent);
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
        const auto parent_id = class_symbol_to_id.at(parent_name);
        const auto class_id = class_symbol_to_id.at(name);
        inheritance_graph[parent_id].push_back(class_id);
        parent[class_id] = parent_id;
    }
    std::vector<bool> visited(class_count());
    std::vector<bool> on_stack(class_count());
    std::vector<int> cycle;
    auto has_cycle = [&](auto&& has_cycle, int u) -> bool {
        if (on_stack[u]) {
            for (auto node = parent[u]; node != u; node = parent[node]) {
                cycle.push_back(node);
            }
            cycle.push_back(u);
            return true;
        }
        if (visited[u]) {
            return false;
        }
        visited[u] = true;
        on_stack[u] = true;
        for (const int v : inheritance_graph[u]) {
            if (has_cycle(has_cycle, v)) {
                break;
            }
        }
        on_stack[u] = false;
        return !cycle.empty();
    };
    for (int u = 0; u < class_count(); ++u) {
        if (!visited[u]) {
            if (has_cycle(has_cycle, u)) {
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

void ClassTable::precompute_lca_lookup_table() {
    std::vector<int> euler_tour;
    std::vector<int> depth;
    lca_last_occurence.resize(class_count());
    auto dfs = [&](auto&& dfs, int u, int p, int d) -> void {
        lca_last_occurence[u] = euler_tour.size();
        euler_tour.push_back(u);
        depth.push_back(d);
        for (const int v : inheritance_graph[u]) {
            if (v != p) {
                dfs(dfs, v, u, d + 1);
            }
        }
        lca_last_occurence[u] = euler_tour.size();
        euler_tour.push_back(u);
        depth.push_back(d);
    };
    dfs(dfs, class_symbol_to_id.at(Object), kNoParent, 0);
    const int euler_tour_sz = euler_tour.size();
    const int sparse_table_sz = std::bit_width((unsigned)euler_tour_sz);
    lca_sparse_table.resize(sparse_table_sz, std::vector<LcaSparseTableNode>(euler_tour_sz));
    for (int i = 0; i < euler_tour_sz; ++i) {
        lca_sparse_table[0][i] = { depth[i], euler_tour[i] };
    }
    for (int i = 1; i < sparse_table_sz; ++i) {
        const int offset = 1 << (i - 1);
        for (int j = 0; j + offset < euler_tour_sz; ++j) {
            lca_sparse_table[i][j] = std::min(lca_sparse_table[i - 1][j], lca_sparse_table[i - 1][j + offset]);
        }
    }
}

int ClassTable::lookup_lca(const std::vector<int>& nodes) {
    int first_idx = class_count(), last_idx = 0;
    for (const auto& node : nodes) {
        first_idx = std::min(first_idx, lca_last_occurence[node]);
        last_idx = std::max(last_idx, lca_last_occurence[node]);
    }
    const auto sparse_table_idx = std::bit_width(last_idx - first_idx + 1u) - 1;
    const auto offset = (1 << sparse_table_idx) - 1;
    return std::min(lca_sparse_table[sparse_table_idx][first_idx], lca_sparse_table[sparse_table_idx][last_idx - offset]).node;
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
                if (!class_map.contains(formal_type)) {
                    semant_error(cls->get_filename(), formal)
                        << "Class " << class_symbol << " has a method " << method_name
                        << " with an undefined formal parameter type " << formal_type << "." << std::endl;
                    return {};
                }
                method_signature.push_back(formal_type);
            }
            const auto method_type = feature->get_type();
            if (!class_map.contains(method_type) && method_type != SELF_TYPE) {
                semant_error(cls->get_filename(), feature)
                    << "Class " << class_symbol << " has a method " << method_name
                    << " with an undefined return type " << method_type << "." << std::endl;
                return {};
            }
            method_signature.push_back(method_type);
            return method_signature;
        }();
        if (method_signature.empty()) {
            continue;
        }
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
    collect_methods(class_symbol_to_id.at(Object), kNoParent);
    if (class_map.contains(Main) && !find_method(class_symbol_to_id.at(Main), main_meth)) {
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

void ClassTable::collect_attributes(int u, int p) {
    const auto class_symbol = class_id_to_symbol[u];
    const auto cls = class_map[class_symbol];
    const auto features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        const auto feature = features->nth(i);
        if (feature->is_method()) {
            continue;
        }
        const auto attr_name = feature->get_name();
        if (attr_name == self) {
            semant_error(cls->get_filename(), feature)
                << "Class " << class_symbol << " cannot use self as an attribute name." << std::endl;
            continue;
        }
        if (const auto res = find_attribute(u, attr_name)) {
            if (res->class_id == u) {
                semant_error(cls->get_filename(), feature)
                    << "Class " << class_symbol << " redefines attribute " << attr_name << "." << std::endl;
            } else {
                semant_error(cls->get_filename(), feature)
                    << "Class " << class_symbol << " redefines attribute " << attr_name
                    << " in parent class " << class_id_to_symbol[res->class_id] << "." << std::endl;
            }
            continue;
        }
        const auto attr_type = feature->get_type();
        if (!is_builtin_class(class_symbol) && !class_map.contains(attr_type) && attr_type != SELF_TYPE) {
            semant_error(cls->get_filename(), feature)
                << "Class " << class_symbol << " has an attribute " << attr_name
                << " with an undefined type " << attr_type << "." << std::endl;
            continue;
        }
        class_attributes[u][attr_name] = attr_type;
    }
    for (const int v : inheritance_graph[u]) {
        if (v != p) {
            collect_attributes(v, u);
        }
    }
}

void ClassTable::collect_attributes() {
    class_attributes.resize(class_count());
    collect_attributes(class_symbol_to_id.at(Object), kNoParent);
    if (semant_debug) {
        for (int u = 0; u < class_count(); ++u) {
            const auto& attrs = class_attributes[u];
            std::cerr << "Class " << class_id_to_symbol[u] << " attributes:\n";
            for (const auto& [attr_name, attr_type] : attrs) {
                std::cerr << "  " << attr_name << " : " << attr_type << "\n";
            }
        }
    }
}

void assign_class::check_types(TypeChecker& type_checker) {
    set_type(Object);
    expr->check_types(type_checker);
    if (name == self) {
        type_checker.get_error_stream(this)
            << "Cannot assign to self." << std::endl;
        return;
    }
    const auto var_type = type_checker.get_symbol_table().lookup(name);
    if (!var_type) {
        type_checker.get_error_stream(this)
            << "Undeclared identifier: " << name << "." << std::endl;
        return;
    }
    if (!type_checker.is_class_conformant(expr->get_type(), var_type)) {
        type_checker.get_error_stream(this)
            << "Cannot assign expression of type " << expr->get_type()
            << " to variable of type " << var_type << "." << std::endl;
        return;
    }
    set_type(expr->get_type());
}

void static_dispatch_class::check_types(TypeChecker& type_checker) {
    set_type(Object);
    if (type_name == SELF_TYPE) {
        type_checker.get_error_stream(this)
            << "Cannot use SELF_TYPE as a static dispatch type." << std::endl;
        return;
    }
    if (!type_checker.find_class(type_name)) {
        type_checker.get_error_stream(this)
            << "Class " << type_name << " is not defined." << std::endl;
        return;
    }
    expr->check_types(type_checker);
    if (!type_checker.is_class_conformant(expr->get_type(), type_name)) {
        type_checker.get_error_stream(this)
            << "Expression of type " << expr->get_type() << " cannot be statically dispatched to class "
            << type_name << "." << std::endl;
        return;
    }
    std::vector<Symbol> arg_types(actual->len());
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        const auto arg = actual->nth(i);
        arg->check_types(type_checker);
        arg_types[i] = arg->get_type();
    }
    const auto method_signature = type_checker.find_method(type_name, name);
    if (method_signature.empty()) {
        type_checker.get_error_stream(this)
            << "Method " << name << " is not defined for class " << expr->get_type() << "." << std::endl;
        return;
    }
    if (method_signature.size() != arg_types.size() + 1) {
        type_checker.get_error_stream(this)
            << "Method " << name << " expects " << method_signature.size() - 1
            << " arguments, but got " << arg_types.size() << "." << std::endl;
        return;
    }
    for (int i = 0; i < actual->len(); ++i) {
        if (!type_checker.is_class_conformant(arg_types[i], method_signature[i])) {
            type_checker.get_error_stream(this)
                << "Argument " << i + 1 << " of method " << name
                << " is of type " << arg_types[i] << ", but expected type is "
                << method_signature[i] << "." << std::endl;
            return;
        }
    }
    set_type(method_signature.back() == SELF_TYPE ? expr->get_type() : method_signature.back());
}

void dispatch_class::check_types(TypeChecker& type_checker) {
    set_type(Object);
    expr->check_types(type_checker);
    std::vector<Symbol> arg_types(actual->len());
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        const auto arg = actual->nth(i);
        arg->check_types(type_checker);
        arg_types[i] = arg->get_type();
    }
    const auto method_signature = type_checker.find_method(expr->get_type(), name);
    if (method_signature.empty()) {
        type_checker.get_error_stream(this)
            << "Method " << name << " is not defined for class " << expr->get_type() << "." << std::endl;
        return;
    }
    if (method_signature.size() != arg_types.size() + 1) {
        type_checker.get_error_stream(this)
            << "Method " << name << " expects " << method_signature.size() - 1
            << " arguments, but got " << arg_types.size() << "." << std::endl;
        return;
    }
    for (int i = 0; i < actual->len(); ++i) {
        if (!type_checker.is_class_conformant(arg_types[i], method_signature[i])) {
            type_checker.get_error_stream(this)
                << "Argument " << i + 1 << " of method " << name
                << " is of type " << arg_types[i] << ", but expected type is "
                << method_signature[i] << "." << std::endl;
            return;
        }
    }
    set_type(method_signature.back() == SELF_TYPE ? expr->get_type() : method_signature.back());
}

void cond_class::check_types(TypeChecker& type_checker) {
    set_type(Object);
    pred->check_types(type_checker);
    if (pred->get_type() != Bool) {
        type_checker.get_error_stream(this)
            << "Predicate in conditional must be of type Bool, but is of type "
            << pred->get_type() << "." << std::endl;
        return;
    }
    then_exp->check_types(type_checker);
    else_exp->check_types(type_checker);
    set_type(type_checker.lub({ then_exp->get_type(), else_exp->get_type() }));
}

void loop_class::check_types(TypeChecker& type_checker) {
    set_type(Object);
    pred->check_types(type_checker);
    if (pred->get_type() != Bool) {
        type_checker.get_error_stream(this)
            << "Predicate in loop must be of type Bool, but is of type "
            << pred->get_type() << "." << std::endl;
        return;
    }
    body->check_types(type_checker);
}

CaseTypes branch_class::check_types(TypeChecker& type_checker) {
    if (name == self) {
        type_checker.get_error_stream(this)
            << "Cannot use self as a case branch variable name." << std::endl;
        return { type_decl, Object };
    }
    if (type_decl == SELF_TYPE) {
        type_checker.get_error_stream(this)
            << "Cannot use SELF_TYPE as a case branch variable type." << std::endl;
        return { type_decl, Object };
    }
    if (!type_checker.find_class(type_decl)) {
        type_checker.get_error_stream(this)
            << "Undeclared class in case branch expression: " << type_decl << "." << std::endl;
        return { type_decl, Object };
    }
    type_checker.get_symbol_table().enterscope();
    type_checker.get_symbol_table().addid(name, type_decl);
    expr->check_types(type_checker);
    type_checker.get_symbol_table().exitscope();
    if (!type_checker.is_class_conformant(expr->get_type(), type_decl)) {
        type_checker.get_error_stream(this)
            << "Expression of type " << expr->get_type()
            << " does not conform to declared type " << type_decl
            << " for case branch variable " << name << "." << std::endl;
        return { type_decl, Object };
    }
    return { type_decl, expr->get_type() };
}

void typcase_class::check_types(TypeChecker& type_checker) {
    set_type(Object);
    expr->check_types(type_checker);
    std::unordered_set<Symbol> case_decl_types;
    std::vector<Symbol> case_expr_types;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        const auto cas = cases->nth(i);
        const auto [case_decl_type, case_expr_type] = cas->check_types(type_checker);
        if (!case_decl_types.insert(case_decl_type).second) {
            type_checker.get_error_stream(cas)
                << "Duplicate case branch for type " << case_decl_type << "." << std::endl;
            return;
        }
        case_expr_types.push_back(case_expr_type);
    }
    set_type(type_checker.lub(case_expr_types));
}

void block_class::check_types(TypeChecker& type_checker) {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        const auto expr = body->nth(i);
        expr->check_types(type_checker);
        set_type(expr->get_type());
    }
}

void let_class::check_types(TypeChecker& type_checker) {
    set_type(Object);
    if (identifier == self) {
        type_checker.get_error_stream(this)
            << "Cannot use self as a variable name in let expression." << std::endl;
        return;
    }
    const Symbol init_type = type_checker.find_class(type_decl);
    if (!init_type) {
        type_checker.get_error_stream(this)
            << "Undeclared class: " << type_decl << "." << std::endl;
        return;
    }
    init->check_types(type_checker);
    if (init->get_type() != No_type && !type_checker.is_class_conformant(init->get_type(), init_type)) {
        type_checker.get_error_stream(this)
            << "Type of initialization expression (" << init->get_type() << ") does not conform to declared type ("
            << type_decl << ") for variable " << identifier << "." << std::endl;
        return;
    }
    type_checker.get_symbol_table().enterscope();
    type_checker.get_symbol_table().addid(identifier, init_type);
    body->check_types(type_checker);
    type_checker.get_symbol_table().exitscope();
    set_type(body->get_type());
}

Symbol check_types_for_arith(tree_node* node, Expression e1, Expression e2, TypeChecker& type_checker) {
    e1->check_types(type_checker);
    e2->check_types(type_checker);
    if (e1->get_type() != Int || e2->get_type() != Int) {
        type_checker.get_error_stream(node)
            << "Both expressions in arithmetic operation must be of type Int, but are of types "
            << e1->get_type() << " and " << e2->get_type() << "." << std::endl;
    }
    return Int;
}

void plus_class::check_types(TypeChecker& type_checker) {
    set_type(check_types_for_arith(this, e1, e2, type_checker));
}

void sub_class::check_types(TypeChecker& type_checker) {
    set_type(check_types_for_arith(this, e1, e2, type_checker));
}

void mul_class::check_types(TypeChecker& type_checker) {
    set_type(check_types_for_arith(this, e1, e2, type_checker));
}

void divide_class::check_types(TypeChecker& type_checker) {
    set_type(check_types_for_arith(this, e1, e2, type_checker));
}

void neg_class::check_types(TypeChecker& type_checker) {
    set_type(Int);
    e1->check_types(type_checker);
    if (e1->get_type() != Int) {
        type_checker.get_error_stream(this)
            << "Expression in '~' must be of type Int, but is of type "
            << e1->get_type() << "." << std::endl;
    }
}

Symbol check_types_for_compare(tree_node* node, Expression e1, Expression e2, TypeChecker& type_checker) {
    e1->check_types(type_checker);
    e2->check_types(type_checker);
    if (e1->get_type() != Int || e2->get_type() != Int) {
        type_checker.get_error_stream(node)
            << "Both expressions in compare operation must be of type Int, but are of types "
            << e1->get_type() << " and " << e2->get_type() << "." << std::endl;
    }
    return Bool;
}

void lt_class::check_types(TypeChecker& type_checker) {
    set_type(check_types_for_compare(this, e1, e2, type_checker));
}

void leq_class::check_types(TypeChecker& type_checker) {
    set_type(check_types_for_compare(this, e1, e2, type_checker));
}

void eq_class::check_types(TypeChecker& type_checker) {
    set_type(Bool);
    e1->check_types(type_checker);
    e2->check_types(type_checker);  
    auto is_not_freely_comparable = [](const Symbol name) {
        return name == Int || name == Bool || name == Str;
    };
    if (is_not_freely_comparable(e1->get_type()) || is_not_freely_comparable(e2->get_type())) {
        if (e1->get_type() != e2->get_type()) {
            type_checker.get_error_stream(this)
                << "Cannot compare types " << e1->get_type() << " and " << e2->get_type() << " for equality." << std::endl;
            return;
        }
    }
}

void comp_class::check_types(TypeChecker& type_checker) {
    set_type(Bool);
    e1->check_types(type_checker);
    if (e1->get_type() != Bool) {
        type_checker.get_error_stream(this)
            << "Expression in 'not' must be of type Bool, but is of type "
            << e1->get_type() << "." << std::endl;
        return;
    }
}

void int_const_class::check_types(TypeChecker& type_checker) {
    set_type(Int);
}

void bool_const_class::check_types(TypeChecker& type_checker) {
    set_type(Bool);
}

void string_const_class::check_types(TypeChecker& type_checker) {
    set_type(Str);
}

void new__class::check_types(TypeChecker& type_checker) {
    if (const auto type = type_checker.find_class(type_name)) {
        set_type(type);
    } else {
        set_type(Object);
        type_checker.get_error_stream(this) << "Undeclared class: " << type_name << "." << std::endl;
    }
}

void isvoid_class::check_types(TypeChecker& type_checker) {
    e1->check_types(type_checker);
    set_type(Bool);
}

void no_expr_class::check_types(TypeChecker& type_checker) {
    set_type(No_type);
}

void object_class::check_types(TypeChecker& type_checker) {
    if (const auto type = type_checker.get_symbol_table().lookup(name)) {
        set_type(type);
    } else {
        set_type(Object);
        type_checker.get_error_stream(this) << "Undeclared identifier: " << name << "." << std::endl;
    }
}

void ClassTable::traverse_class_hierarchy(int class_id, int parent_id) {
    current_class = class_map.at(class_id_to_symbol[class_id]);
    class_map[SELF_TYPE] = current_class;
    symbol_table.enterscope();
    for (const auto [attr_name, attr_type] : class_attributes[class_id]) {
        symbol_table.addid(attr_name, attr_type);
    }
    symbol_table.addid(self, current_class->get_name());
    const auto features = current_class->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        const auto feature = features->nth(i);
        const auto feature_type = find_class(feature->get_type());
        if (feature->is_method()) {
            symbol_table.enterscope();
            const auto formals = feature->get_formals();
            for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
                const auto formal = formals->nth(j);
                symbol_table.addid(formal->get_name(), formal->get_type());
            }
            feature->get_expr()->check_types(*this);
            symbol_table.exitscope();
            if (feature_type && !is_class_conformant(feature->get_expr()->get_type(), feature_type)) {
                semant_error(current_class->get_filename(), feature)
                    << "The method " << feature->get_name() << " in class " << current_class->get_name()
                    << " has body type " << feature->get_expr()->get_type()
                    << " which does not conform to its declared return type "
                    << feature->get_type() << "." << std::endl;
            }
        } else {
            feature->get_expr()->check_types(*this);
            if (feature_type && !is_class_conformant(feature->get_expr()->get_type(), feature_type)) {
                semant_error(current_class->get_filename(), feature)
                    << "The attribute " << feature->get_name() << " in class " << current_class->get_name()
                    << " has initialization expression type " << feature->get_expr()->get_type()
                    << " which does not conform to its declared type "
                    << feature->get_type() << "." << std::endl;
            }
        }
    }
    for (const int child_id : inheritance_graph[class_id]) {
        if (child_id != parent_id) {
            traverse_class_hierarchy(child_id, class_id);
        }
    }
    symbol_table.exitscope();
};

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


