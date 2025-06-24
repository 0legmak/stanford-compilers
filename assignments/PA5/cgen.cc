
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <algorithm>
#include <stack>
#include <vector>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
extern int cgen_optimize;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

StringEntryP empty_string;
IntEntryP zero_int;

using Register::ZERO;
using Register::AT;
using Register::V0;
using Register::V1;
using Register::A0;
using Register::A1;
using Register::A2;
using Register::A3;
using Register::T0;
using Register::T1;
using Register::T2;
using Register::T3;
using Register::T4;
using Register::T5;
using Register::T6;
using Register::T7;
using Register::S0;
using Register::S1;
using Register::S2;
using Register::S3;
using Register::S4;
using Register::S5;
using Register::S6;
using Register::S7;
using Register::T8;
using Register::T9;
using Register::K0;
using Register::K1;
using Register::GP;
using Register::SP;
using Register::FP;
using Register::RA;
using Register::ACC;
using Register::SELF;

constexpr const char* rn(Register reg) {
  switch (reg) {
    case ZERO: return "$zero";
    case AT:   return "$at";
    case V0:   return "$v0";
    case V1:   return "$v1";
    case A0:   return "$a0";
    case A1:   return "$a1";
    case A2:   return "$a2";
    case A3:   return "$a3";
    case T0:   return "$t0";
    case T1:   return "$t1";
    case T2:   return "$t2";
    case T3:   return "$t3";
    case T4:   return "$t4";
    case T5:   return "$t5";
    case T6:   return "$t6";
    case T7:   return "$t7";
    case S0:   return "$s0";
    case S1:   return "$s1";
    case S2:   return "$s2";
    case S3:   return "$s3";
    case S4:   return "$s4";
    case S5:   return "$s5";
    case S6:   return "$s6";
    case S7:   return "$s7";
    case T8:   return "$t8";
    case T9:   return "$t9";
    case K0:   return "$k0";
    case K1:   return "$k1";
    case GP:   return "$gp";
    case SP:   return "$sp";
    case FP:   return "$fp";
    case RA:   return "$ra";
    default:   return "<unknown>";
  }
}

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

  empty_string = stringtable.add_string("");
  zero_int = inttable.add_string("0");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(Register dest_reg, int offset, Register source_reg, ostream& s)
{
  s << LW << rn(dest_reg) << " " << offset * WORD_SIZE << "(" << rn(source_reg) << ")" 
    << ENDL;
}

static void emit_store(Register source_reg, int offset, Register dest_reg, ostream& s)
{
  s << SW << rn(source_reg) << " " << offset * WORD_SIZE << "(" << rn(dest_reg) << ")"
      << ENDL;
}

static void emit_load_imm(Register dest_reg, int val, ostream& s)
{ s << LI << rn(dest_reg) << " " << val << ENDL; }

static void emit_load_address(Register dest_reg, char *address, ostream& s)
{ s << LA << rn(dest_reg) << " " << address << ENDL; }

static void emit_partial_load_address(Register dest_reg, ostream& s)
{ s << LA << rn(dest_reg) << " "; }

static void emit_load_bool(Register dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << "  # " << (b.get_val() ? "true" : "false") << ENDL;
}

static std::string escape_string(const std::string& input) {
  std::string result;
  for (char c : input) {
    switch (c) {
      case '\n': result += "\\n"; break;
      case '\t': result += "\\t"; break;
      case '\r': result += "\\r"; break;
      case '\\': result += "\\\\"; break;
      case '\"': result += "\\\""; break;
      case '\b': result += "\\b"; break;
      case '\f': result += "\\f"; break;
      default:
        if (static_cast<unsigned char>(c) < 32 || static_cast<unsigned char>(c) == 127) {
          char buf[5];
          snprintf(buf, sizeof(buf), "\\x%02x", static_cast<unsigned char>(c));
          result += buf;
        } else {
          result += c;
        }
    }
  }
  return result;
}

static void emit_load_string(Register dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << "  # \"" << escape_string(str->get_string()) << "\"" << ENDL;
}

static void emit_load_int(Register dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << "  # " << i->get_string() << ENDL;
}

static void emit_move(Register dest_reg, Register source_reg, ostream& s) {
  if (dest_reg == source_reg) {
    return;
  }
  s << MOVE << rn(dest_reg) << " " << rn(source_reg) << ENDL;
}

static void emit_neg(Register dest, Register src1, ostream& s)
{ s << NEG << rn(dest) << " " << rn(src1) << ENDL; }

static void emit_add(Register dest, Register src1, Register src2, ostream& s)
{ s << ADD << rn(dest) << " " << rn(src1) << " " << rn(src2) << ENDL; }

static void emit_addu(Register dest, Register src1, Register src2, ostream& s)
{ s << ADDU << rn(dest) << " " << rn(src1) << " " << rn(src2) << ENDL; }

static void emit_addiu(Register dest, Register src, int imm, ostream& s)
{ s << ADDIU << rn(dest) << " " << rn(src) << " " << imm << ENDL; }

static void emit_div(Register dest, Register src1, Register src2, ostream& s)
{ s << DIV << rn(dest) << " " << rn(src1) << " " << rn(src2) << ENDL; }

static void emit_mul(Register dest, Register src1, Register src2, ostream& s)
{ s << MUL << rn(dest) << " " << rn(src1) << " " << rn(src2) << ENDL; }

static void emit_sub(Register dest, Register src1, Register src2, ostream& s)
{ s << SUB << rn(dest) << " " << rn(src1) << " " << rn(src2) << ENDL; }

static void emit_sll(Register dest, Register src1, int num, ostream& s)
{ s << SLL << rn(dest) << " " << rn(src1) << " " << num << ENDL; }

static void emit_jalr(Register dest, ostream& s)
{ s << JALR << rn(dest) << ENDL; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << ENDL; }

static void emit_return(ostream& s)
{ s << JR << rn(RA) << ENDL; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << ENDL; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << ENDL;
}

static void emit_beqz(Register source, int label, ostream &s)
{
  s << BEQZ << rn(source) << " ";
  emit_label_ref(label,s);
  s << ENDL;
}

static void emit_beq(Register src1, Register src2, int label, ostream &s)
{
  s << BEQ << rn(src1) << " " << rn(src2) << " ";
  emit_label_ref(label,s);
  s << ENDL;
}

static void emit_bne(Register src1, Register src2, int label, ostream &s)
{
  s << BNE << rn(src1) << " " << rn(src2) << " ";
  emit_label_ref(label,s);
  s << ENDL;
}

static void emit_bleq(Register src1, Register src2, int label, ostream &s)
{
  s << BLEQ << rn(src1) << " " << rn(src2) << " ";
  emit_label_ref(label,s);
  s << ENDL;
}

static void emit_blt(Register src1, Register src2, int label, ostream &s)
{
  s << BLT << rn(src1) << " " << rn(src2) << " ";
  emit_label_ref(label,s);
  s << ENDL;
}

static void emit_blti(Register src1, int imm, int label, ostream &s)
{
  s << BLT << rn(src1) << " " << imm << " ";
  emit_label_ref(label,s);
  s << ENDL;
}

static void emit_bgti(Register src1, int imm, int label, ostream &s)
{
  s << BGT << rn(src1) << " " << imm << " ";
  emit_label_ref(label,s);
  s << ENDL;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << ENDL;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(Register reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(Register dest, Register source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(Register source, Register dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }

static void emit_fetch_bool(Register dest, Register source, ostream& s) {
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

static void emit_slti(Register dest, Register source, int imm, ostream& s) {
  s << SLTI << rn(dest) << " " << rn(source) << " " << imm << ENDL;
}

static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << ENDL;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(Register source, ostream &s)
{
  if (source != A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << ENDL;
}

constexpr int kIntValAttr = 0;

void emit_fetch_attr(const SymbolLocation src, int attr_idx, Register dst, ostream &s) {
  if (src.offset) {
    emit_load(dst, *src.offset, src.reg, s);
    emit_load(dst, DEFAULT_OBJFIELDS + attr_idx, dst, s);
  } else {
    emit_load(dst, DEFAULT_OBJFIELDS + attr_idx, src.reg, s);
  }
}

void emit_copy_to_reg(const SymbolLocation src, Register dst, ostream &s) {
  if (src.offset) {
    emit_load(dst, *src.offset, src.reg, s);
  } else {
    emit_move(dst, src.reg, s);
  }
}

static bool is_attr_location(const SymbolLocation loc) {
  return loc.offset && loc.reg == SELF;
}

void emit_assign(const Register src, const SymbolLocation dst, ostream& s) {
  if (dst.offset) {
    emit_store(src, *dst.offset, dst.reg, s);
    if (cgen_Memmgr != GC_NOGC && is_attr_location(dst)) {
      emit_addiu(A1, dst.reg, *dst.offset * WORD_SIZE, s);
      emit_gc_assign(s);
    }
  } else {
    emit_move(dst.reg, src, s);
  }
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << ENDL;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << ENDL                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << ENDL // size
      << WORD;


 /***** Add dispatch information for class String ******/

      emit_disptable_ref(Str, s);                             // dispatch table
      s << ENDL;
      s << WORD;  lensym->code_ref(s);  s << ENDL;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << ENDL;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << ENDL                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << ENDL  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      emit_disptable_ref(Int, s);                         // dispatch table
      s << ENDL;
      s << WORD << str << ENDL;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << ENDL;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << ENDL                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << ENDL   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      emit_disptable_ref(Bool, s);                          // dispatch table
      s << ENDL;
      s << WORD << val << ENDL;                             // value (0 or 1)
}

bool BoolConst::get_val() const {
  return !!val;
}

bool method_class::is_method() {
  return true;
}

bool attr_class::is_method() {
  return false;
}

Symbol method_class::get_type() {
  return return_type;
}

Symbol attr_class::get_type() {
  return type_decl;
}

Expression method_class::get_expr() {
  return expr;
}

Expression attr_class::get_expr() {
  return init;
}

Formals method_class::get_formals() {
  return formals;
}

Formals attr_class::get_formals() {
  return nullptr;
}


//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << ENDL;
  str << GLOBAL; emit_protobj_ref(main,str);    str << ENDL;
  str << GLOBAL; emit_protobj_ref(integer,str); str << ENDL;
  str << GLOBAL; emit_protobj_ref(string,str);  str << ENDL;
  str << GLOBAL; falsebool.code_ref(str);  str << ENDL;
  str << GLOBAL; truebool.code_ref(str);   str << ENDL;
  str << GLOBAL << INTTAG << ENDL;
  str << GLOBAL << BOOLTAG << ENDL;
  str << GLOBAL << STRINGTAG << ENDL;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << ENDL;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << ENDL;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << ENDL;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << ENDL
      << HEAP_START << LABEL 
      << WORD << 0 << ENDL
      << "\t.text" << ENDL
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << ENDL << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << ENDL << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << ENDL << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << ENDL << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << ENDL;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << ENDL;
  str << "_MemMgr_INITIALIZER:" << ENDL;
  str << WORD << gc_init_names[cgen_Memmgr] << ENDL;
  str << GLOBAL << "_MemMgr_COLLECTOR" << ENDL;
  str << "_MemMgr_COLLECTOR:" << ENDL;
  str << WORD << gc_collect_names[cgen_Memmgr] << ENDL;
  str << GLOBAL << "_MemMgr_TEST" << ENDL;
  str << "_MemMgr_TEST:" << ENDL;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << ENDL;
}

void CgenClassTable::code_dispatch_table_and_prototype_objects() {
  std::unordered_map<Symbol, int> method_to_index;
  struct DispTableEntry {
    Symbol class_name;
    Symbol method_name;
  };
  std::vector<DispTableEntry> disp_table;
  enum UndoAction {
    kAddEntry,
    kUpdateEntry
  };
  struct UndoRecord {
    UndoAction action;
    int disp_table_index;
    Symbol prev_class_name;
  };
  std::stack<UndoRecord> undo_stack;
  std::vector<Symbol> attr_types;
  auto process_class = [&](auto&& process_class, CgenNode* node) -> void {
    const auto class_name = node->get_name();
    const auto features = node->get_features();
    const auto prev_undo_stack_size = undo_stack.size();
    const auto prev_attr_types_size = attr_types.size();
    auto& method_table_for_class = method_table[class_name];
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      const auto feature = features->nth(i);
      if (feature->is_method()) {
        const auto method_name = feature->get_name();
        int disp_table_index = disp_table.size();
        if (auto ins_res = method_to_index.insert({ method_name, disp_table_index }); ins_res.second) {
          undo_stack.push({ kAddEntry });
          disp_table.push_back({ class_name, method_name });
        } else {
          disp_table_index = ins_res.first->second;
          undo_stack.push({ kUpdateEntry, disp_table_index, disp_table[disp_table_index].class_name });
          disp_table[disp_table_index].class_name = class_name;
        }
        const auto formals = feature->get_formals();
        std::vector<Symbol> arg_names(formals->len());
        for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
          arg_names[j] = formals->nth(j)->get_name();
        }
        method_table_for_class[method_name] = { disp_table_index, std::move(arg_names) };
      } else {
        attr_types.push_back(feature->get_type());
      }
    }

    emit_disptable_ref(class_name, str); str << LABEL;
    for (const auto& [class_name, method_name] : disp_table) {
      str << WORD; emit_method_ref(class_name, method_name, str); str << ENDL;
    }

    str << WORD << "-1" << ENDL;
    emit_protobj_ref(class_name, str); str << LABEL;
    str << WORD << node->get_class_tag() << ENDL;
    str << WORD << (DEFAULT_OBJFIELDS + attr_types.size()) << ENDL;
    str << WORD; emit_disptable_ref(class_name, str); str << ENDL;
    for (const auto attr_type : attr_types) {
      str << WORD;
      if (attr_type == Int) {
        zero_int->code_ref(str);
      } else if (attr_type == Bool) {
        falsebool.code_ref(str);
      } else if (attr_type == Str) {
        empty_string->code_ref(str);
      } else {
        str << "0";
      }
      str << ENDL;
    }

    for (List<CgenNode>* child = node->get_children(); child; child = child->tl()) {
      process_class(process_class, child->hd());
    }

    attr_types.resize(prev_attr_types_size);
    while (undo_stack.size() != prev_undo_stack_size) {
      const auto& undo_record = undo_stack.top();
      switch (undo_record.action) {
        case kAddEntry:
          method_to_index.erase(disp_table.back().method_name);
          disp_table.pop_back();
          break;
        case kUpdateEntry:
          disp_table[undo_record.disp_table_index].class_name = undo_record.prev_class_name;
          break;
      }
      undo_stack.pop();
    }
  };
  process_class(process_class, root());
}

void CgenClassTable::code_class_name_and_object_tables() {
  const int list_size = list_length(nds);
  std::vector<StringEntryP> class_str_table(list_size);
  std::vector<Symbol> class_sym_table(list_size);
  for (List<CgenNode>* l = nds; l; l = l->tl()) {
    CgenNode* node = l->hd();
    class_str_table[node->get_class_tag()] = stringtable.lookup_string(node->get_name()->get_string());
    class_sym_table[node->get_class_tag()] = node->get_name();
  }
  str << CLASSNAMETAB << LABEL;
  for (const auto entry : class_str_table) {
    str << WORD;
    entry->code_ref(str);
    str << "  # \"" << escape_string(entry->get_string()) << "\"" << ENDL;
  }
  str << CLASSOBJTAB << LABEL;
  for (const auto entry : class_sym_table) {
    str << WORD; emit_protobj_ref(entry, str); str << ENDL;
    str << WORD; emit_init_ref(entry, str); str << ENDL;
  }
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   assign_class_tags();
   prepare_registers_for_temporaries();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenClassTable::assign_class_tags() {
  int class_tag = 0;
  for (List<CgenNode>* l = nds; l; l = l->tl()) {
    CgenNode* node = l->hd();
    node->set_class_tag(class_tag++);
  }
  stringclasstag = probe(Str)->get_class_tag();
  intclasstag = probe(Int)->get_class_tag();
  boolclasstag = probe(Bool)->get_class_tag();
}

void CgenClassTable::prepare_registers_for_temporaries() {
  if (!cgen_optimize) {
    return;
  }
  registers_for_temporaries = { S1, S2, S3, S4, S5, S6 };
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  code_dispatch_table_and_prototype_objects();
  code_class_name_and_object_tables();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  code_methods();
  code_dispatch_abort_handlers();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   class_tag(-1)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

int CgenClassTable::create_label() {
  return label_id++;
}

static std::ostream& operator<<(std::ostream& os, const SymbolLocation& loc) {
  if (loc.offset) {
    os << *loc.offset << "(" << rn(loc.reg) << ")";
  } else {
    os << rn(loc.reg);
  }
  return os;
}

SymbolLocation CgenClassTable::allocate_temporary(Register value_reg) {
  SymbolLocation loc;
  if (registers_used_cnt < registers_for_temporaries.size()) {
    loc = { registers_for_temporaries[registers_used_cnt] };
    ++registers_used_cnt;
    emit_move(loc.reg, value_reg, str);
  } else {
    loc = { FP, curr_fp_offset };
    --curr_fp_offset;
    emit_store(value_reg, *loc.offset, loc.reg, str);
  }
  temporaries_stack.push(loc);
  temporaries_used = std::max(temporaries_used, temporaries_stack.size());
  if (cgen_debug) {
    std::cerr << "Allocated temporary at " << loc << ", current FP offset: " << curr_fp_offset << std::endl;
  }
  return loc;
}

void CgenClassTable::free_temporary() {
  const SymbolLocation loc = temporaries_stack.top();
  temporaries_stack.pop();
  if (loc.offset) {
    if (loc.reg != FP || loc.offset != curr_fp_offset + 1) {
      throw std::logic_error((std::ostringstream() << "Bad attempt to free temporary at "
        << loc << ", current FP offset: " << curr_fp_offset).str());
    }
    ++curr_fp_offset;
  } else {
    if (registers_used_cnt == 0 || loc.reg != registers_for_temporaries[registers_used_cnt - 1]) {
      throw std::logic_error((std::ostringstream() << "Bad attempt to free temporary at "
        << loc << ", current used register count : " << registers_used_cnt).str());
    }
    --registers_used_cnt;
  }
  if (cgen_debug) {
    std::cerr << "Deallocated temporary at " << loc << ", current FP offset: " << curr_fp_offset << std::endl;
  }
}

SymbolLocation CgenClassTable::get_symbol_location(Symbol name) {
  const auto loc = symbol_environment.at(name).top();
  if (cgen_debug) {
    std::cerr << "Found symbol " << name << " at " << loc << std::endl;
  }
  return loc;
}

void CgenClassTable::push_symbol_location(Symbol name, SymbolLocation loc) {
  stack_symbols.push(name);
  auto& locs = symbol_environment[name];
  locs.push(loc);
  if (cgen_debug) {
    std::cerr << "Created symbol " << name << " at " << loc << std::endl;
  }
}

void CgenClassTable::pop_symbol_location() {
  auto& locs = symbol_environment.at(stack_symbols.top());
  if (cgen_debug) {
    std::cerr << "Deleted symbol " << stack_symbols.top() << " at " << locs.top() << std::endl;
  }
  locs.pop();
  stack_symbols.pop();
}

FindMethodResult CgenClassTable::find_method(Symbol class_name, Symbol method_name) {
  for (
    auto class_node = class_name == SELF_TYPE ? current_class_node : probe(class_name);
    class_node;
    class_node = class_node->get_parentnd()
  ) {
    const auto& class_method_table = method_table[class_node->get_name()];
    const auto iter = class_method_table.find(method_name);
    if (iter != class_method_table.end()) {
      return { class_node->get_name(), iter->second.dispatch_table_index, iter->second.arg_names };
    }
  }
  throw std::runtime_error(
    std::string("Method ") + method_name->get_string() + " not found in class " + class_name->get_string()
  );
}

std::vector<int> CgenClassTable::create_jump_table(const std::vector<Symbol>& types) {
  const int class_count = list_length(nds);
  std::vector<int> used_type_indices(class_count, -1);
  for (size_t i = 0; i < types.size(); ++i) {
    const auto type = types[i];
    const auto class_node = probe(type);
    used_type_indices[class_node->get_class_tag()] = i;
  }
  std::vector<int> jump_table(class_count, -1);
  auto dfs = [&](auto&& dfs, CgenNode* node, int closest_ancestor) -> void {
    if (used_type_indices[node->get_class_tag()] != -1) {
      closest_ancestor = used_type_indices[node->get_class_tag()];
    }
    jump_table[node->get_class_tag()] = closest_ancestor;
    for (List<CgenNode>* child = node->get_children(); child; child = child->tl()) {
      dfs(dfs, child->hd(), closest_ancestor);
    }
  };
  dfs(dfs, root(), -1);
  return jump_table;
}

char* CgenClassTable::get_filename() {
  return current_class_node->get_filename()->get_string();
}


struct AnnotateImpl : public Annotate {
  AnnotateImpl(std::ostream& s, const std::string& message, int line_number, int& indent)
    : s(s), message(message), line_number(line_number), indent(indent)
  {
    s << "#\t\t\t\t" << std::string(indent, ' ') << "{ " << message << " at line " << line_number << ENDL;
    indent += 2;
  }
  ~AnnotateImpl() override {
    indent -= 2;
    s << "#\t\t\t\t" << std::string(indent, ' ') << "} " << message << " at line " << line_number << ENDL;
  }
  std::ostream& s;
  const std::string message;
  int line_number;
  int& indent;
};

std::unique_ptr<Annotate> CgenClassTable::annotate(const std::string& message, int line_number) {
  return std::make_unique<AnnotateImpl>(str, message, line_number, annotation_indent);
}

int CgenClassTable::get_dispatch_abort_label(StringEntryP file_name, int line_number) {
  const auto iter = dispatch_abort_labels.insert({{ file_name, line_number }, -1}).first;
  if (iter->second == -1) {
    iter->second = create_label();
  }
  return iter->second;
}

void CgenClassTable::code_methods() {
  push_symbol_location(self, { SELF });
  std::vector<Symbol> attr_names;
  std::vector<Expression> attr_exprs;
  auto process_class = [&](auto&& process_class, CgenNode* node) -> void {
    current_class_node = node;
    const auto class_name = node->get_name();
    const auto features = node->get_features();
    const auto prev_attrs_size = attr_exprs.size();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      const auto feature = features->nth(i);
      if (!feature->is_method()) {
        attr_names.push_back(feature->get_name());
        attr_exprs.push_back(feature->get_expr());
      }
    }
    const int attr_count = attr_names.size();
    for (int i = 0; i < attr_count; ++i) {
      push_symbol_location(attr_names[i], { SELF, DEFAULT_OBJFIELDS + i });
    }

    auto emit_method = [&](int arg_cnt, auto&& emit_header, auto&& emit_body) {
      reset_stats();
      str.set_enabled(false);
      emit_body();
      str.set_enabled(true);
      emit_header();
      curr_fp_offset = 0;
      emit_store(FP, curr_fp_offset--, SP, str);
      emit_move(FP, SP, str);
      emit_store(RA, curr_fp_offset--, FP, str);
      emit_store(SELF, curr_fp_offset--, FP, str);
      const auto saved_register_cnt = std::min(temporaries_used, registers_for_temporaries.size());
      for (size_t i = 0; i < saved_register_cnt; ++i) {
        emit_store(registers_for_temporaries[i], curr_fp_offset--, FP, str);
      }
      const auto temp_on_stack_count = temporaries_used - saved_register_cnt;
      if (cgen_Memmgr != GC_NOGC) {
        for (size_t i = 0; i < temp_on_stack_count; ++i) {
          emit_store(ZERO, curr_fp_offset - i, FP, str);
        }
      }
      emit_addiu(SP, SP, (curr_fp_offset + -temp_on_stack_count) * WORD_SIZE, str);
      emit_move(SELF, ACC, str);
      emit_body();
      emit_addiu(SP, SP, (-curr_fp_offset + temp_on_stack_count + arg_cnt) * WORD_SIZE, str);
      for (size_t i = 0; i < saved_register_cnt; ++i) {
        emit_load(registers_for_temporaries[saved_register_cnt - 1 - i], ++curr_fp_offset, FP, str);
      }
      emit_load(SELF, ++curr_fp_offset, FP, str);
      emit_load(RA, ++curr_fp_offset, FP, str);
      emit_load(FP, ++curr_fp_offset, FP, str);
      if (curr_fp_offset != 0) {
        throw std::logic_error("Wrong FP offset: " + std::to_string(curr_fp_offset));
      }
      emit_return(str);
    };

    emit_method(
      0,
      [&]() {
        emit_init_ref(class_name, str); str << LABEL;
      },
      [&]() {
        for (int i = 0; i < attr_count; ++i) {
          if (attr_exprs[i]->get_type() && attr_exprs[i]->get_type() != No_type) {
            const auto attr_expr_res = attr_exprs[i]->code(str, *this);
            emit_assign(attr_expr_res.REG, { SELF, DEFAULT_OBJFIELDS + i }, str);
          }
        }
        emit_move(ACC, SELF, str);
      }
    );

    if (!node->basic()) {
      for (int i = features->first(); features->more(i); i = features->next(i)) {
        const auto feature = features->nth(i);
        if (!feature->is_method()) {
          continue;
        }
        const auto formals = feature->get_formals();
        const auto arg_cnt = formals->len();
        for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
          push_symbol_location(formals->nth(j)->get_name(), { FP, arg_cnt - j });
        }
        emit_method(
          arg_cnt,
          [&]() {
            emit_method_ref(class_name, feature->get_name(), str); str << LABEL;
          },
          [&]() {
            const auto expr_res = feature->get_expr()->code(str, *this);
            emit_move(ACC, expr_res.REG, str);
          }
        );
        for (int j = 0; j < arg_cnt; ++j) {
          pop_symbol_location();
        }
      }
    }

    for (List<CgenNode>* child = node->get_children(); child; child = child->tl()) {
      process_class(process_class, child->hd());
    }

    for (size_t i = prev_attrs_size; i < attr_names.size(); ++i) {
      pop_symbol_location();
    }
    attr_names.resize(prev_attrs_size);
    attr_exprs.resize(prev_attrs_size);
  };
  process_class(process_class, root());
}

void CgenClassTable::code_dispatch_abort_handlers() {
  for (const auto [entry, label] : dispatch_abort_labels) {
    const auto [file_name, line_number] = entry;
    emit_label_def(label, str);
    emit_load_imm(T1, line_number, str);
    emit_load_string(ACC, file_name, str);
    emit_jal("_dispatch_abort", str);
  }
}

CodeResult assign_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate(std::string("assign '") + name->get_string() + "'", line_number);
  const auto expr_res = expr->code(s, codegen);
  emit_assign(expr_res.REG, codegen.get_symbol_location(name), s);
  return expr_res;
}

CodeResult code_dispatch(
  bool dynamic,
  Expression expr,
  Symbol type,
  Symbol method_name,
  Expressions args,
  int line_number,
  ostream &s,
  CodeGenerator& codegen
) {
  const auto [class_name, disp_table_index, arg_names] =
    codegen.find_method(dynamic ? expr->get_type() : type, method_name);
  const auto arg_cnt = arg_names.size();
  for (int i = args->first(); args->more(i); i = args->next(i)) {
    const auto arg_expr = args->nth(i);
    const auto arg_expr_res = arg_expr->code(s, codegen);
    emit_push(arg_expr_res.REG, s);
  }
  const auto expr_res = expr->code(s, codegen);
  const auto dispatch_abort_label = codegen.get_dispatch_abort_label(
    stringtable.lookup_string(codegen.get_filename()), line_number
  );
  emit_beqz(expr_res.REG, dispatch_abort_label, s);
  emit_move(ACC, expr_res.REG, s);
  if (dynamic) {
    emit_load(T1, DISPTABLE_OFFSET, ACC, s);
    emit_load(T1, disp_table_index, T1, s);
    emit_jalr(T1, s);
  } else {
    s << JAL; emit_method_ref(class_name, method_name, s); s << ENDL;
  }
  return CodeResult{};
}

CodeResult static_dispatch_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate(std::string(type_name->get_string()) + "." + name->get_string() + "()", line_number);
  return code_dispatch(false, expr, type_name, name, actual, line_number, s, codegen);
}

CodeResult dispatch_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate(std::string(name->get_string()) + "()", line_number);
  return code_dispatch(true, expr, nullptr, name, actual, line_number, s, codegen);
}

CodeResult cond_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("if", line_number);
  const auto label_if_false = codegen.create_label();
  const auto label_end = codegen.create_label();
  const auto pred_res = pred->code(s, codegen);
  emit_fetch_bool(ACC, pred_res.REG, s);
  emit_beqz(ACC, label_if_false, s);
  const auto then_exp_res = then_exp->code(s, codegen);
  emit_move(ACC, then_exp_res.REG, s);
  emit_branch(label_end, s);
  emit_label_def(label_if_false, s);
  const auto else_exp_res = else_exp->code(s, codegen);
  emit_move(ACC, else_exp_res.REG, s);
  emit_label_def(label_end, s);
  return CodeResult{};
}

CodeResult loop_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("loop", line_number);
  const auto label_start = codegen.create_label();
  const auto label_end = codegen.create_label();
  emit_label_def(label_start, s);
  const auto pred_res = pred->code(s, codegen);
  emit_fetch_bool(ACC, pred_res.REG, s);
  emit_beqz(ACC, label_end, s);
  const auto body_res = body->code(s, codegen);
  emit_branch(label_start, s);
  emit_label_def(label_end, s);
  emit_move(ACC, ZERO, s);
  return CodeResult{};
}

CodeResult typcase_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("case", line_number);
  const int cases_count = cases->len();
  std::vector<Symbol> types(cases_count);
  std::vector<int> case_labels(cases_count);
  std::vector<Expression> case_expr(cases_count);
  std::vector<Symbol> case_vars(cases_count);
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    const auto cas = cases->nth(i);
    types[i] = cas->get_type();
    case_labels[i] = codegen.create_label();
    case_expr[i] = cas->get_expr();
    case_vars[i] = cas->get_name();
  }
  const auto jump_table = codegen.create_jump_table(types);
  const auto jump_table_label = codegen.create_label();
  const auto case_end_label = codegen.create_label();
  const auto case_abort_label = codegen.create_label();
  const auto case_abort2_label = codegen.create_label();
  
  const auto expr_res = expr->code(s, codegen);
  emit_beqz(expr_res.REG, case_abort2_label, s);
  const auto case_var_loc = codegen.allocate_temporary(expr_res.REG);
  emit_load(ACC, TAG_OFFSET, expr_res.REG, s);
  emit_sll(ACC, ACC, 2, s);
  emit_partial_load_address(T1, s); emit_label_ref(jump_table_label, s); s << ENDL;
  emit_addu(ACC, T1, ACC, s);
  emit_load(ACC, 0, ACC, s);
  emit_jalr(ACC, s);
  
  for (int i = 0; i < cases_count; ++i) {
    emit_label_def(case_labels[i], s);
    codegen.push_symbol_location(case_vars[i], case_var_loc);
    const auto case_expr_res = case_expr[i]->code(s, codegen);
    emit_move(ACC, case_expr_res.REG, s);
    codegen.pop_symbol_location();
    emit_branch(case_end_label, s);
  }

  emit_label_def(case_abort_label, s);
  emit_copy_to_reg(case_var_loc, ACC, s);
  emit_jal("_case_abort", s);

  emit_label_def(case_abort2_label, s);
  emit_load_imm(T1, line_number, s);
  emit_load_string(ACC, stringtable.lookup_string(codegen.get_filename()), s);  
  emit_jal("_case_abort2", s);

  s << ALIGN;
  emit_label_def(jump_table_label, s);
  for (size_t i = 0; i < jump_table.size(); ++i) {
    s << WORD;
    emit_label_ref(jump_table[i] != -1 ? case_labels[jump_table[i]] : case_abort_label, s);
    s << ENDL;
  }

  emit_label_def(case_end_label, s);
  codegen.free_temporary();
  return CodeResult{};
}

CodeResult block_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("block", line_number);
  CodeResult code_res;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    const auto expr = body->nth(i);
    code_res = expr->code(s, codegen);
  }
  return code_res;
}

CodeResult let_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate(std::string("let '") + identifier->get_string() + ":" + type_decl->get_string() + "'", line_number);
  Register init_reg;
  if (init->get_type() && init->get_type() != No_type) {
    init_reg = init->code(s, codegen).REG;
  } else {
    if (type_decl == Int) {
      emit_load_int(ACC, zero_int, s);
    } else if (type_decl == Bool) {
      emit_load_bool(ACC, falsebool, s);
    } else if (type_decl == Str) {
      emit_load_string(ACC, empty_string, s);
    } else {
      emit_move(ACC, ZERO, s);
    }
    init_reg = ACC;
  }
  const auto init_var_loc = codegen.allocate_temporary(init_reg);
  codegen.push_symbol_location(identifier, init_var_loc);
  const auto body_res = body->code(s, codegen);
  codegen.pop_symbol_location();
  codegen.free_temporary();
  return body_res;
}

static void create_object(Symbol type, ostream &s) {
  emit_partial_load_address(ACC, s); emit_protobj_ref(type, s); s << ENDL;
  emit_jal(OBJECT_COPY, s);
  s << JAL; emit_init_ref(type, s); s << ENDL;
}

template <auto emit_arith_op>
CodeResult code_arith(Expression e1, Expression e2, ostream &s, CodeGenerator& codegen) {
  const auto e1_res = e1->code(s, codegen);
  const auto e1_loc = codegen.allocate_temporary(e1_res.REG);
  const auto e2_res = e2->code(s, codegen);
  const auto e2_loc = codegen.allocate_temporary(e2_res.REG);
  create_object(Int, s);
  emit_fetch_attr(e2_loc, kIntValAttr, T2, s);
  codegen.free_temporary();
  emit_fetch_attr(e1_loc, kIntValAttr, T1, s);
  codegen.free_temporary();
  emit_arith_op(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  return CodeResult{};
}

CodeResult plus_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("plus", line_number);
  return code_arith<emit_add>(e1, e2, s, codegen);
}

CodeResult sub_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("sub", line_number);
  return code_arith<emit_sub>(e1, e2, s, codegen);
}

CodeResult mul_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("mul", line_number);
  return code_arith<emit_mul>(e1, e2, s, codegen);
}

CodeResult divide_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("div", line_number);
  return code_arith<emit_div>(e1, e2, s, codegen);
}

CodeResult neg_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("neg", line_number);
  const auto e1_res = e1->code(s, codegen);
  const auto e1_loc = codegen.allocate_temporary(e1_res.REG);
  create_object(Int, s);
  emit_fetch_attr(e1_loc, kIntValAttr, T1, s);
  codegen.free_temporary();
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
  return CodeResult{};
}

CodeResult eq_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("eq", line_number);
  const auto e1_res = e1->code(s, codegen);
  const auto e1_loc = codegen.allocate_temporary(e1_res.REG);
  const auto e2_res = e2->code(s, codegen);
  emit_move(T2, e2_res.REG, s);
  emit_copy_to_reg(e1_loc, T1, s);
  codegen.free_temporary();
  const auto eq_label = codegen.create_label();
  emit_load_bool(ACC, truebool, s);
  emit_beq(T1, T2, eq_label, s);
  emit_load_bool(A1, falsebool, s);
  emit_jal("equality_test", s);
  emit_label_def(eq_label, s);
  return CodeResult{};
}

template<auto emit_cmp_branch>
CodeResult code_comparison(Expression e1, Expression e2, ostream &s, CodeGenerator& codegen) {
  const auto e1_res = e1->code(s, codegen);
  const auto e1_loc = codegen.allocate_temporary(e1_res.REG);
  const auto e2_res = e2->code(s, codegen);
  emit_fetch_int(ACC, e2_res.REG, s);
  emit_fetch_attr(e1_loc, kIntValAttr, T1, s);
  codegen.free_temporary();
  const auto label_if_true = codegen.create_label();
  const auto label_end = codegen.create_label();
  emit_cmp_branch(T1, ACC, label_if_true, s);
  emit_load_bool(ACC, falsebool, s);
  emit_branch(label_end, s);
  emit_label_def(label_if_true, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(label_end, s);
  return CodeResult{};
}

CodeResult lt_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("lt", line_number);
  return code_comparison<emit_blt>(e1, e2, s, codegen);
}

CodeResult leq_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("leq", line_number);
  return code_comparison<emit_bleq>(e1, e2, s, codegen);
}

CodeResult comp_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("not", line_number);
  const auto e1_res = e1->code(s, codegen);
  emit_fetch_bool(ACC, e1_res.REG, s);
  emit_slti(ACC, ACC, 1, s);
  const auto label_if_zero = codegen.create_label();
  const auto label_end = codegen.create_label();
  emit_beqz(ACC, label_if_zero, s);
  emit_load_bool(ACC, truebool, s);
  emit_branch(label_end, s);
  emit_label_def(label_if_zero, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label_end, s);
  return CodeResult{};
}

CodeResult int_const_class::code(ostream& s, CodeGenerator& codegen) {
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
  return CodeResult{};
}

CodeResult string_const_class::code(ostream& s, CodeGenerator& codegen) {
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
  return CodeResult{};
}

CodeResult bool_const_class::code(ostream& s, CodeGenerator& codegen) {
  emit_load_bool(ACC, BoolConst(val), s);
  return CodeResult{};
}

CodeResult new__class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate(std::string("new '") + type_name->get_string() + "'", line_number);
  if (type_name == SELF_TYPE) {
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, TAG_OFFSET, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    emit_load(ACC, 1, T1, s);
    const auto init_loc = codegen.allocate_temporary(ACC);
    emit_load(ACC, 0, T1, s);
    emit_jal(OBJECT_COPY, s);
    if (init_loc.offset) {
      emit_copy_to_reg(init_loc, T1, s);
      emit_jalr(T1, s);
    } else {
      emit_jalr(init_loc.reg, s);
    }
    codegen.free_temporary();
  } else {
    create_object(type_name, s);
  }
  return CodeResult{};
}

CodeResult isvoid_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate("isvoid", line_number);
  const auto e1_res = e1->code(s, codegen);
  const auto label_if_zero = codegen.create_label();
  const auto label_end = codegen.create_label();
  emit_beqz(e1_res.REG, label_if_zero, s);
  emit_load_bool(ACC, falsebool, s);
  emit_branch(label_end, s);
  emit_label_def(label_if_zero, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(label_end, s);
  return CodeResult{};
}

CodeResult no_expr_class::code(ostream &s, CodeGenerator& codegen) {
  throw std::logic_error("no_expr must not participate code generation");
}

CodeResult object_class::code(ostream &s, CodeGenerator& codegen) {
  auto a = codegen.annotate(std::string("object '") + name->get_string() + "'", line_number);
  const auto loc = codegen.get_symbol_location(name);
  if (loc.offset) {
    emit_copy_to_reg(loc, ACC, s);
    return CodeResult{};
  }
  if (cgen_optimize) {
    return { loc.reg };
  }
  emit_move(ACC, loc.reg, s);
  return CodeResult{};
}
