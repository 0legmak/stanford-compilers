class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
	other(a : Int) : C { self };
};

Class Main {
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};

class A inherits B {
};

class B inherits D {
};

class D inherits A {
};

class Q inherits None {};

class MyInt inherits Int {};

class C {};

class S inherits S {};

class MethodTest inherits C {
	dup() : SELF_TYPE { self };
	dup() : SELF_TYPE { self };
	init(x : Int, y : Bool) : C { self };
	other(a : String) : C { self };
	dup_formal(a : Int, a : Int) : SELF_TYPE { self };
	self_formal(self : Int) : SELF_TYPE { self };
	self_type_formal(a : SELF_TYPE) : SELF_TYPE { self };
};

class AttrTest inherits C {
	a : Int;
	noType : NoType;
	x : String;
	x : String;
};
