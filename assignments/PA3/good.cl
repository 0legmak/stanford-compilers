class A {
    ana(): Int {
        {
            let io:IO <- new IO in
                case self of
                    a : A => io.out_string("Class type is A\n");
                    b : BB__ => io.out_string("Class type is BB__\n");
                    o : Object => io.out_string("Oooops\n");
                esac;
            3 + (let
                x:Int <- ~~~1,
                xvoid:String
            in
                let
                    y:Int <- 1,
                    yvoid:String,
                    z:Int <- 1
                in
                    x + y + z);
        }
    };
};

Class BB__ inherits A {
};

class Main inherits IO {
    main(): SELF_TYPE {
        {
            self.out_int((new A).ana());
            self.out_string("\n");
        }
    };
};
