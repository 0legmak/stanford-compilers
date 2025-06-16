
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
  int: Int;
  bool: Bool;
  str: String;
  obj1: B <- new B;
  obj2: C;
  f() : Int { 1 };
  main():SELF_TYPE {{
    let io: IO <- new IO in {
      io.out_int(int);
      --io.out_int(b);
      io.out_string(str);
    };
    self;
  }};
};

