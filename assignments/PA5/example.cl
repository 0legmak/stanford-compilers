
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
  str: String <- "\n";
  obj1: B <- new B;
  obj2: C;
  f() : Int { 1 };

  recurse(s: String) : Bool {{
    let io: IO <- new IO in {
      io.out_string(s);
      io.out_string(" ");
    };
    if s.length() = 0 then true else recurse(s.substr(0, s.length() - 1)) fi;
  }};

  main():Int {{
    let io: IO <- new IO in {
      case new SELF_TYPE of
        int: Int => io.out_int(int);
        str: String => io.out_string(str);
        obj1: Object => io.out_string(obj1.type_name());
      esac;
      io.out_string("\n");
      let v: Int <- 2 * 4 in {
        v <- v * (v + 1) / 2;
        io.out_int(v);
        io.out_string("\n");
        if (v < v) then io.out_string("v < v is true\n") else io.out_string("v < v is false\n") fi;
        if (v <= v) then io.out_string("v <= v is true\n") else io.out_string("v <= v is false\n") fi;
        if (v = v) then io.out_string("v = v is true\n") else io.out_string("v = v is false\n") fi;
        if v = 10 then io.out_string("v = 10 is true\n") else io.out_string("v = 10 is false\n") fi;
      };
      let v: Int in {
        if not isvoid v then io.out_string("void\n") else io.out_string("not void\n") fi;
      };
      io.out_string((new SELF_TYPE).type_name());
      io.out_string("\n");
      let i: Int <- 10 in
        while not(i = 0) loop {
          i <- i - 1;
          io.out_int(i);
          io.out_string(" ");
        } pool;
      io.out_string("\n");
      io.out_string("testtest".substr(2, 4));
      io.out_string("\n");
      recurse("abcde");
      io.out_string("\n");
    };
    0;
  }};
};

