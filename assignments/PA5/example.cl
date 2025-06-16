
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class B {
  f() : Int { 0 };
  g() : Int { 0 };
};

class C inherits B {
  g() : Int { 1 };
};

class Main inherits C {
  f() : Int { 1 };
  main():Int { 0 };
};

