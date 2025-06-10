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
