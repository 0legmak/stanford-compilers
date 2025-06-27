-- https://en.wikipedia.org/wiki/Lehmer_random_number_generator#Sample_C99_code

class ParkMillerLCG {
    state: Int;

    init(s: Int): SELF_TYPE {{
        state <- s;
        self;
    }};

    next(): Int {
        let m: Int <- 2147483647,         -- 0x7fffffff
            a: Int <- 48271,
            q: Int <- m / a,              -- 44488
            r: Int <- m - q * a,          -- 3399
            div: Int <- state / q,        -- max: M / Q = A = 48,271
            rem: Int <- state - div * q,  -- max: Q - 1     = 44,487
            s: Int <- rem * a,            -- max: 44,487 * 48,271 = 2,147,431,977 = 0x7fff3629
            t: Int <- div * r,            -- max: 48,271 *  3,399 =   164,073,129
            result: Int <- s - t
        in {
            if result < 0 then
                result <- result + m
            else
                0
            fi;
            state <- result;
        }
    };
};

-- https://cp-algorithms.com/data_structures/treap.html
-- https://ru.algorithmica.org/cs/tree-structures/implicit/

class Node {
    value: Int;
    size: Int;
    prior: Int;
    l: Node;
    r: Node;

    init(v: Int, p: Int): SELF_TYPE {{
        value <- v;
        size <- 1;
        prior <- p;
        self;
    }};

    get_value(): Int { value };
    set_value(v: Int): Int { value <- v };
    get_size(): Int { size };
    set_size(s: Int): Int { size <- s };
    get_prior(): Int { prior };
    get_l(): Node { l };
    set_l(v: Node): Node { l <- v };
    get_r(): Node { r };
    set_r(v: Node): Node { r <- v };
};

class NodePair {
    l: Node;
    r: Node;

    init(left: Node, right: Node): SELF_TYPE {{
        l <- left;
        r <- right;
        self;
    }};
    get_l(): Node { l };
    get_r(): Node { r };
};

class Treap {
    rng: ParkMillerLCG <- (new ParkMillerLCG).init(54645645);
    root: Node;

    size_(v: Node): Int { if isvoid v then 0 else v.get_size() fi };

    upd_(v: Node): Int { v.set_size(1 + size_(v.get_l()) + size_(v.get_r())) };

    merge_(l: Node, r: Node): Node {
        if isvoid l then r
        else if isvoid r then l
        else if r.get_prior() < l.get_prior() then {
            l.set_r(merge_(l.get_r(), r));
            upd_(l);
            l;
        } else {
            r.set_l(merge_(l, r.get_l()));
            upd_(r);
            r;
        }
        fi fi fi
    };

    split_(v: Node, idx: Int): NodePair {
        if isvoid v then new NodePair
        else if size_(v.get_l()) < idx then
            let pair: NodePair <- split_(v.get_r(), idx - size_(v.get_l()) - 1) in {
                v.set_r(pair.get_l());
                upd_(v);
                (new NodePair).init(v, pair.get_r());
            }
        else
            let pair: NodePair <- split_(v.get_l(), idx) in {
                v.set_l(pair.get_r());
                upd_(v);
                (new NodePair).init(pair.get_l(), v);
            }
        fi fi
    };

    get_(v: Node, idx: Int): Node {
        if idx < size_(v.get_l()) then
            get_(v.get_l(), idx)
        else if size_(v.get_l()) < idx then
            get_(v.get_r(), idx - size_(v.get_l()) - 1)
        else
            v
        fi fi
    };

    insert_(v: Node, idx: Int, n: Node): Node {
        if isvoid v then n
        else if v.get_prior() < n.get_prior() then
            let pair: NodePair <- split_(v, idx) in {
                n.set_l(pair.get_l());
                n.set_r(pair.get_r());
                upd_(n);
                n;
            }
        else if size_(v.get_l()) < idx then {
            v.set_r(insert_(v.get_r(), idx - size_(v.get_l()) - 1, n));
            upd_(v);
            v;
        } else {
            v.set_l(insert_(v.get_l(), idx, n));
            upd_(v);
            v;
        }
        fi fi fi
    };

    erase_(v: Node, idx: Int): Node {
        if idx < size_(v.get_l()) then {
            v.set_l(erase_(v.get_l(), idx));
            upd_(v);
            v;
        } else if size_(v.get_l()) < idx then {
            v.set_r(erase_(v.get_r(), idx - size_(v.get_l()) - 1));
            upd_(v);
            v;
        } else {
            merge_(v.get_l(), v.get_r());
        }
        fi fi
    };

    size(): Int { size_(root) };

    get(idx: Int): Int { get_(root, idx).get_value() };

    set(idx: Int, val: Int): Int { get_(root, idx).set_value(val) };

    insert(idx: Int, val: Int): Node {
        root <- insert_(root, idx, (new Node).init(val, rng.next()))
    };

    erase(idx: Int): Node { root <- erase_(root, idx) };
};

-- Complete binary tree

class BstNode {
    value: Int;
    size: Int;
    l: BstNode;
    r: BstNode;

    init(v: Int): SELF_TYPE {{
        value <- v;
        size <- 1;
        self;
    }};

    get_value(): Int { value };
    set_value(v: Int): Int { value <- v };
    get_size(): Int { size };
    set_size(s: Int): Int { size <- s };
    get_l(): BstNode { l };
    set_l(v: BstNode): BstNode { l <- v };
    get_r(): BstNode { r };
    set_r(v: BstNode): BstNode { r <- v };
};

class CompleteBinaryTree {
    root: BstNode;

    size_(v: BstNode): Int { if isvoid v then 0 else v.get_size() fi };

    upd_(v: BstNode): Int { v.set_size(1 + size_(v.get_l()) + size_(v.get_r())) };

    build_(index: Int, size: Int, init_val: Int): BstNode {
        let new_node: BstNode <- (new BstNode).init(init_val),
            left_index: Int <- index * 2 + 1,
            right_index: Int <- index * 2 + 2
        in {
            if left_index < size then {
                new_node.set_l(build_(left_index, size, init_val)); 0;
            } else 0 fi;
            if right_index < size then {
                new_node.set_r(build_(right_index, size, init_val)); 0;
            } else 0 fi;
            upd_(new_node);
            new_node;
        }
    };

    get_(v: BstNode, idx: Int): BstNode {
        if idx < size_(v.get_l()) then
            get_(v.get_l(), idx)
        else if size_(v.get_l()) < idx then
            get_(v.get_r(), idx - size_(v.get_l()) - 1)
        else
            v
        fi fi
    };

    init(size: Int, init_val: Int): SELF_TYPE {{
        if 0 < size then
            root <- build_(0, size, init_val)
        else root fi;
        self;
    }};

    size(): Int { size_(root) };

    get(idx: Int): Int { get_(root, idx).get_value() };

    set(idx: Int, val: Int): Int { get_(root, idx).set_value(val) };
};

class Main inherits IO {
    main(): SELF_TYPE {{
        out_string("Max number to test?\n");
        let max_n: Int <- in_int(),
            is_prime: CompleteBinaryTree <- (new CompleteBinaryTree).init(max_n + 1, 1),
            i: Int,
            cnt: Int,
            io: IO <- new IO
        in {
            is_prime.set(0, 0);
            is_prime.set(1, 0);
            i <- 2;
            while i <= max_n / i loop {
                if is_prime.get(i) = 1 then {
                    cnt <- cnt + 1;
                    io.out_int(i);
                    io.out_string(" ");
                    let j: Int <- i * i in {
                        while j <= max_n loop {
                            is_prime.set(j, 0);
                            j <- j + i;
                        } pool;
                        0;
                    };
                } else 0 fi;
                i <- i + 1;
            } pool;
            while i <= max_n loop {
                if is_prime.get(i) = 1 then {
                    cnt <- cnt + 1;
                    io.out_int(i);
                    io.out_string(" ");
                    0;
                } else 0 fi;
                i <- i + 1;
            } pool;
            io.out_string("\nFound ");
            io.out_int(cnt);
            io.out_string(" primes\n");
        };
        self;
    }};
};
