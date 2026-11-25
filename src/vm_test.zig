const std = @import("std");
const test_utils = @import("test_utils.zig");
const virtual_machine = @import("vm.zig");
const chunck = @import("chunk.zig");

const DEBUG = true;

const TestCase = struct {
    description: []const u8,
    source: []const u8,
};

const run = struct {
    fn runTest(test_case: TestCase) anyerror!void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const result = try virtual_machine.run(std.testing.io, std.testing.allocator, arena.allocator(), "test", test_case.source);
        try std.testing.expect(result == .Ok);
    }
}.runTest;

test "vm tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "variable declaration",
            .source =
            \\a = 6
            \\b = 7
            \\c = a + b
            \\assert(c == 13)
            ,
        },
        .{
            .description = "shadowing",
            .source =
            \\a = 6
            \\a = 11
            \\b = 11
            \\c = a + b
            \\assert(c == 22)
            ,
        },
        .{
            .description = "nested variable declaration",
            .source =
            \\a = 6
            \\b = 7
            \\c = a + b
            \\(
            \\    d = c + 1
            \\    assert(d == 14)
            \\)
            \\assert(c == 13)
            ,
        },
        .{
            .description = "using grouping in declaration",
            .source =
            \\a = (
            \\    b = 1
            \\    b + 1
            \\)
            \\assert(a == 2)
            ,
        },
        .{
            .description = "using grouping in binary expression and declaration",
            .source =
            \\a = (
            \\    b = 1
            \\    b + 1
            \\) + 2
            \\assert(a == 4)
            ,
        },
        .{
            .description = "nested grouping",
            .source =
            \\a = (
            \\    b = 1
            \\    (
            \\        c = 2
            \\        c + b
            \\    )
            \\) + 2
            \\assert(a == 5)
            ,
        },
        .{
            .description = "logical and true",
            .source =
            \\a = true and true
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "logical and false",
            .source =
            \\a = false and true
            \\
            \\assert(a == false)
            ,
        },
        .{
            .description = "logical and false on right hand side",
            .source =
            \\a = true and false
            \\
            \\assert(a == false)
            ,
        },
        .{
            .description = "logical or false",
            .source =
            \\a = false or false
            \\
            \\assert(a == false)
            ,
        },
        .{
            .description = "logical or true both",
            .source =
            \\a = true or true
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "logical or true left",
            .source =
            \\a = true or false
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "logical or true right",
            .source =
            \\a = false or true
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "binary and unary",
            .source =
            \\assert((5 + 5) == 10)
            \\assert((10 - 20) == -10)
            \\assert((10 * 54) == 540)
            \\assert((2.5 * 10.0) == 25.0)
            \\assert((20.0 * 4.5) == 90.0)
            \\assert((10 / 5) == 2)
            \\assert((10.0 / 2.5) == 4.0)
            \\assert((20.5 / 2.0) == 10.25)
            \\assert((100 % 30) == 10)
            \\assert((30 % 100) == 30)
            \\assert((1 & 1) == 1)
            \\assert((1 & 0) == 0)
            \\assert((1 | 1) == 1)
            \\assert((1 | 0) == 1)
            \\assert((1 ^ 1) == 0)
            \\assert((0 ^ 1) == 1)
            \\assert((2 << 2) == 8)
            \\assert((16 >> 2) == 4)
            \\assert((~10) == -11)
            \\assert((5.5 % 3.1) == 2.4)
            \\assert((5 > 3) == true)
            \\assert((4 < 10) == true)
            \\assert((3 <= 3) == true)
            \\assert((2 <= 3) == true)
            \\assert((2 >= 2) == true)
            \\assert((2 >= 1) == true)
            ,
        },
        .{
            .description = "precedence",
            .source =
            \\assert(2 + 3 * 4 == 14)
            \\assert(2 * 3 + 4 == 10)
            \\assert(2 * (3 + 4) == 14)
            \\assert(10 - 3 - 2 == 5)
            \\assert(2 + 3 * 4 - 1 == 13)
            \\assert(10 / 2 + 3 == 8)
            \\assert(10 / (2 + 3) == 2)
            \\assert(2 * 3 * 4 == 24)
            \\assert(true and false or true == true)
            \\assert(true or false and false == true)
            \\assert(!false and true == true)
            \\assert(1 | 2 | 4 == 7)
            \\assert(7 & 6 & 4 == 4)
            \\assert(1 << 1 << 1 == 4)
            \\assert(16 >> 1 >> 1 == 4)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate vm tests", &test_cases, run);
}

test "if tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "evaluate to if",
            .source =
            \\foo = true
            \\bar = if foo then 42 else 43
            \\assert(bar == 42)
            ,
        },
        .{
            .description = "evaluate to else",
            .source =
            \\foo = false
            \\bar = if foo then 42 else 43
            \\assert(bar == 43)
            ,
        },
        .{
            .description = "evaluate to elif",
            .source =
            \\foo = 43
            \\bar = if foo == 42 then 1
            \\      elif foo == 43 then 2
            \\      else 3
            \\assert(bar == 2)
            ,
        },
        .{
            .description = "evaluate to if with many branches",
            .source =
            \\foo = 42
            \\bar = if foo == 42 then 1
            \\      elif foo == 43 then 2
            \\      elif foo == 44 then 3
            \\      elif foo == 45 then 4
            \\      else 5
            \\assert(bar == 1)
            ,
        },
        .{
            .description = "evaluate to else with many branches",
            .source =
            \\foo = 41
            \\bar = if foo == 42 then 1
            \\      elif foo == 43 then 2
            \\      elif foo == 44 then 3
            \\      elif foo == 45 then 4
            \\      else 5
            \\assert(bar == 5)
            ,
        },
        .{
            .description = "implicit else",
            .source =
            \\foo = 41
            \\bar = if foo == 42 then assert(true)
            \\assert(bar == ())
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate function tests", &test_cases, run);
}

test "match tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "one line",
            .source =
            \\foo = match 42 with | 42 -> true
            \\assert(foo == true)
            ,
        },
        .{
            .description = "second branch",
            .source =
            \\foo = match 42 with
            \\| 41 -> 1
            \\| 42 -> 2
            \\assert(foo == 2)
            ,
        },
        .{
            .description = "wildcard at end",
            .source =
            \\foo = match 43 with
            \\| 41 -> 1
            \\| 42 -> 2
            \\| _ -> 3
            \\assert(foo == 3)
            ,
        },
        .{
            .description = "wildcard at start",
            .source =
            \\foo = match 42 with
            \\| _ -> 3
            \\| 42 -> 1
            \\| 43 -> 2
            \\assert(foo == 3)
            ,
        },
        .{
            .description = "wildcard in middle",
            .source =
            \\foo = match 42 with
            \\| 43 -> 1
            \\| _ -> 3
            \\| 42 -> 2
            \\assert(foo == 3)
            ,
        },
        .{
            .description = "fallthrough",
            .source =
            \\foo = match 41 with
            \\| 42 -> assert(true)
            \\| 43 -> assert(true)
            \\assert(foo == ())
            ,
        },
        .{
            .description = "nested",
            .source =
            \\foo = 96
            \\bar = match 43 with
            \\| 42 -> 1
            \\| 43 -> (match foo with
            \\    | 95 -> 2
            \\    | 96 -> 3
            \\    | 97 -> 4)
            \\| 44 -> 5
            \\assert(bar == 3)
            ,
        },
        .{
            .description = "jump over nested",
            .source =
            \\foo = 96
            \\bar = match 44 with
            \\| 42 -> 1
            \\| 43 -> (match foo with
            \\    | 95 -> 2
            \\    | 96 -> 3
            \\    | 97 -> 4)
            \\| 44 -> 5
            \\assert(bar == 5)
            ,
        },
        .{
            .description = "grouping in arm",
            .source =
            \\foo = match 43 with
            \\| 42 -> 1
            \\| 43 -> (bar = 2
            \\     bar + 1)
            \\| 44 -> 5
            \\assert(foo == 3)
            ,
        },
        .{
            .description = "match on empty list",
            .source =
            \\list = []
            \\foo = match list with
            \\| [] -> 2
            \\assert(foo == 2)
            ,
        },
        .{
            .description = "match on cons pattern",
            .source =
            \\list = [1, 2, 3]
            \\foo = match list with
            \\| [] -> false
            \\| h :: t -> h == 1 and list_len(t) == 2
            \\assert(foo == true)
            \\assert(list_len(list) == 3)
            ,
        },
        .{
            .description = "sum list",
            .source =
            \\sum = (list) ->
            \\    match list with
            \\    | [] -> 0
            \\    | h :: t -> h + sum(t)
            \\result = sum([1, 2, 3])
            \\assert(result == 6)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate function tests", &test_cases, run);
}

test "function tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "unit return",
            .source =
            \\foo = () -> assert(true)
            \\result = foo()
            \\assert(result == ())
            ,
        },
        .{
            .description = "generic add function",
            .source =
            \\foo = (a, b) -> a + b
            \\a = foo(33, 32)
            \\b = foo(1.0, 1.5)
            \\c = foo("foo", "bar")
            \\assert(a == 65)
            \\assert(b == 2.5)
            \\assert(c == "foobar")
            ,
        },
        .{
            .description = "grouping return",
            .source =
            \\foo = (a, b) -> (
            \\    c = a + b
            \\    c * c
            \\)
            \\result = foo(3, 4)
            \\assert(result == 49)
            ,
        },
        .{
            .description = "nested function",
            .source =
            \\foo = () -> (
            \\    bar = (a, b) -> a * b
            \\    bar(1, 2) * 3
            \\)
            \\result = foo()
            \\assert(result == 6)
            ,
        },
        .{
            .description = "closure",
            .source =
            \\foo = () -> (
            \\    x = 2
            \\    (a) -> a * x
            \\)
            \\func = foo()
            \\result = func(10)
            \\assert(result == 20)
            ,
        },
        .{
            .description = "deep closure",
            .source =
            \\foo = () -> (
            \\    x = 1
            \\    bar = () -> (
            \\        y = 2
            \\        baz = () -> (
            \\            z = 3
            \\            x + y + z
            \\        )
            \\        baz
            \\    )
            \\    bar
            \\)
            \\
            \\bar = foo()
            \\baz = bar()
            \\result = baz()
            \\
            \\assert(result == 6)
            ,
        },
        .{
            .description = "closure in grouping",
            .source =
            \\foo = () -> (
            \\    bar = (
            \\        a = 2
            \\        (b) -> a + b
            \\    )
            \\    bar
            \\)
            \\func = foo()
            \\result = func(10)
            \\assert(result == 12)
            ,
        },
        .{
            .description = "iife",
            .source =
            \\foo = ((a, b) -> a + b)(1, 2)
            \\assert(foo == 3)
            ,
        },
        .{
            .description = "recursion",
            .source =
            \\fac = (n) -> if n <= 1 then 1 else n * fac(n-1)
            \\result = fac(5)
            \\assert(result == 120)
            ,
        },
        .{
            .description = "recursion with acc",
            .source =
            \\fac = (val) -> (
            \\    inner = (n, acc) -> if n <= 1 then acc else inner(n-1, acc*n)
            \\    inner(val, 1)
            \\)
            \\result = fac(5)
            \\assert(result == 120)
            ,
        },
        .{
            .description = "fib",
            .source =
            \\fib = (index) -> (
            \\    inner = (a, b, n) -> if n <= 0 then a else inner(b, a+b, n-1)
            \\    inner(0, 1, index)
            \\)
            \\result = fib(5)
            \\assert(result == 5)
            ,
        },
        .{
            .description = "tail recursion",
            .source =
            \\count_up = (n, acc) -> if n <= 0 then acc else count_up(n-1, acc+1)
            \\result = count_up(5000, 0)
            \\assert(result == 5000)
            ,
        },
        .{
            .description = "default param",
            .source =
            \\add = (a=1) -> a + 2
            \\result = add()
            \\assert(result == 3)
            ,
        },
        .{
            .description = "mixed params param",
            .source =
            \\f = (a, b = 1, c = 2) -> a + b + c
            \\a = f(10)
            \\b = f(10, 20)
            \\c = f(10, 20, 30)
            \\assert(a == 13)
            \\assert(b == 32)
            \\assert(c == 60)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate function tests", &test_cases, run);
}

test "record tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "literal access",
            .source =
            \\foo = { a = 1 }.a
            \\assert(foo == 1)
            ,
        },
        .{
            .description = "var access",
            .source =
            \\foo = { a = 1 }
            \\bar = foo.a
            \\assert(bar == 1)
            ,
        },
        .{
            .description = "record with var as value",
            .source =
            \\foo = (a) -> { first = a }
            \\result = foo(1)
            \\assert(result.first == 1)
            ,
        },
        .{
            .description = "access in generic function",
            .source =
            \\foo = (r) -> r.a
            \\result = foo({ a = 1 })
            \\assert(result == 1)
            ,
        },
        .{
            .description = "access in expression in generic function",
            .source =
            \\foo = (r) -> r.a + r.b
            \\result = foo({ a = 1, b = 2 })
            \\assert(result == 3)
            ,
        },
        .{
            .description = "multiple calls with different records",
            .source =
            \\foo = (r) -> r.b
            \\result = foo({ a = 1, b = 2 }) + foo({ b = 3, c = 4 })
            \\assert(result == 5)
            ,
        },
        .{
            .description = "equality",
            .source =
            \\a = { foo = 1, bar = 2 }
            \\ref = a
            \\b = { foo = 1, bar = 2 }
            \\c = { foo = 2, bar = 3 }
            \\e = { bar = 2, foo = 1 }
            \\assert(a == ref and ref == a)
            \\assert(a == b and b == a)
            \\assert(a != c and c != a)
            \\assert(a == e and e == a)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate record tests", &test_cases, run);
}

test "string tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "slice",
            .source =
            \\a = "foofoo"
            \\b = str_slice(a, 0, 3)
            \\c = str_slice(a, 3, str_len(a))
            \\assert(b == c)
            ,
        },
        .{
            .description = "equal",
            .source =
            \\assert("foo" == "foo")
            ,
        },
        .{
            .description = "not equal",
            .source =
            \\assert("foo" != "bar")
            ,
        },
        .{
            .description = "greater",
            .source =
            \\assert("foo" > "bar")
            ,
        },
        .{
            .description = "less",
            .source =
            \\assert("bar" < "foo")
            ,
        },
        .{
            .description = "length",
            .source =
            \\assert(str_len("foobar") == 6)
            ,
        },
        .{
            .description = "concatenation",
            .source =
            \\assert("foo" + "bar" == "foobar")
            ,
        },
        .{
            .description = "starts with",
            .source =
            \\assert(str_starts_with("foobar", "foo"))
            ,
        },
        .{
            .description = "not starts with",
            .source =
            \\assert(!str_starts_with("foobar", "bar"))
            ,
        },
        .{
            .description = "ends with",
            .source =
            \\assert(str_ends_with("foobar", "bar"))
            ,
        },
        .{
            .description = "not ends with",
            .source =
            \\assert(!str_ends_with("foobar", "foo"))
            ,
        },
        .{
            .description = "contains",
            .source =
            \\assert(str_contains("foobar", "ob"))
            ,
        },
        .{
            .description = "not contains",
            .source =
            \\assert(!str_contains("foobar", "barfoo"))
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate string tests", &test_cases, run);
}

test "number tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "float from int",
            .source =
            \\assert(float_from_int(5) == 5.0)
            ,
        },
        .{
            .description = "int from float",
            .source =
            \\assert(int_from_float(5.312) == 5)
            ,
        },
        .{
            .description = "equality",
            .source =
            \\assert(1 == 1)
            \\assert(1 != 2)
            \\assert(1.0 == 1.0)
            \\assert(1.0 != 2.3)
            ,
        },
        .{
            .description = "equality upvalue",
            .source =
            \\foo = () -> (
            \\    x = 1
            \\    () -> x
            \\)
            \\func = foo()
            \\result = func()
            \\print(result == 1)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate number tests", &test_cases, run);
}

test "list tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "empty list",
            .source =
            \\foo = []
            \\l = list_len(foo)
            \\assert(l == 0)
            ,
        },
        .{
            .description = "with items",
            .source =
            \\foo = [1, 2, 3]
            \\assert(list_get(foo, 0) == 1)
            \\assert(list_get(foo, 1) == 2)
            \\assert(list_get(foo, 2) == 3)
            \\assert(list_len(foo) == 3)
            ,
        },
        .{
            .description = "cons with empty list",
            .source =
            \\foo = 1 :: []
            \\assert(list_get(foo, 0) == 1)
            \\assert(list_len(foo) == 1)
            ,
        },
        .{
            .description = "cons with filled list",
            .source =
            \\foo = 1 :: [2]
            \\assert(list_get(foo, 0) == 1)
            \\assert(list_get(foo, 1) == 2)
            \\assert(list_len(foo) == 2)
            ,
        },
        .{
            .description = "nested cons",
            .source =
            \\foo = 1 :: (2 :: [])
            \\assert(list_len(foo) == 2)
            \\assert(list_get(foo, 0) == 1)
            \\assert(list_get(foo, 1) == 2)
            ,
        },
        .{
            .description = "immutable list",
            .source =
            \\foo = [1]
            \\bar = 2 :: foo
            \\
            \\assert(list_len(foo) == 1)
            \\assert(list_get(foo, 0) == 1)
            \\
            \\assert(list_len(bar) == 2)
            \\assert(list_get(bar, 0) == 2)
            \\assert(list_get(bar, 1) == 1)
            ,
        },
        .{
            .description = "reverse",
            .source =
            \\foo = [1, 2, 3]
            \\bar = list_reverse(foo)
            \\assert(list_get(foo, 0) == 1)
            \\assert(list_get(foo, 1) == 2)
            \\assert(list_get(foo, 2) == 3)
            \\assert(list_get(bar, 0) == 3)
            \\assert(list_get(bar, 1) == 2)
            \\assert(list_get(bar, 2) == 1)
            ,
        },
        .{
            .description = "map with lambda",
            .source =
            \\map = (list, fn, acc) -> 
            \\    match list with
            \\    | [] -> list_reverse(acc)
            \\    | h :: t -> map(t, fn, fn(h) :: acc)
            \\
            \\new = map([1, 2, 3], (n) -> n * n, [])
            \\
            \\assert(list_get(new, 0) == 1)
            \\assert(list_get(new, 1) == 4)
            \\assert(list_get(new, 2) == 9)
            ,
        },
        .{
            .description = "map with named function",
            .source =
            \\map = (list, fn, acc) ->
            \\    match list with
            \\    | [] -> list_reverse(acc)
            \\    | h :: t -> map(t, fn, fn(h) :: acc)
            \\
            \\square = (n) -> n * n
            \\new = map([1, 2, 3], square, [])
            \\
            \\assert(list_get(new, 0) == 1)
            \\assert(list_get(new, 1) == 4)
            \\assert(list_get(new, 2) == 9)
            ,
        },
        .{
            .description = "filter with lambda",
            .source =
            \\filter = (list, fn, acc) -> 
            \\    match list with
            \\    | [] -> list_reverse(acc)
            \\    | h :: t -> filter(t, fn, if fn(h) then h :: acc else acc)
            \\
            \\new = filter([1, 2, 3, 4], (n) -> n % 2 == 0, [])
            \\assert(list_len(new) == 2)
            \\assert(list_get(new, 0) == 2)
            \\assert(list_get(new, 1) == 4)
            ,
        },
        .{
            .description = "filter with named function",
            .source =
            \\filter = (list, fn, acc) -> 
            \\    match list with
            \\    | [] -> list_reverse(acc)
            \\    | h :: t -> filter(t, fn, if fn(h) then h :: acc else acc)
            \\
            \\is_even = (n) -> n % 2 == 0
            \\new = filter([1, 2, 3, 4], is_even, [])
            \\assert(list_len(new) == 2)
            \\assert(list_get(new, 0) == 2)
            \\assert(list_get(new, 1) == 4)
            ,
        },
        .{
            .description = "filter with lambda and records",
            .source =
            \\filter = (list, fn, acc) -> 
            \\    match list with
            \\    | [] -> list_reverse(acc)
            \\    | h :: t -> filter(t, fn, if fn(h) then h :: acc else acc)
            \\
            \\new = filter([{ a = 1 }, { a = 2 }], (r) -> r.a % 2 == 0, [])
            \\assert(list_len(new) == 1)
            \\assert(list_get(new, 0).a == 2)
            ,
        },
        .{
            .description = "map",
            .source =
            \\new = list_map([1, 2, 3], (n) -> n * 2, [])
            \\assert(list_len(new) == 3)
            \\assert(list_get(new, 0) == 2)
            \\assert(list_get(new, 1) == 4)
            \\assert(list_get(new, 2) == 6)
            ,
        },
        .{
            .description = "map with default arg",
            .source =
            \\new = list_map([1, 2, 3], (n) -> n * 2)
            \\assert(list_len(new) == 3)
            \\assert(list_get(new, 0) == 2)
            \\assert(list_get(new, 1) == 4)
            \\assert(list_get(new, 2) == 6)
            ,
        },
        .{
            .description = "filter",
            .source =
            \\new = list_filter([1, 2, 3], (n) -> n % 2 == 0, [])
            \\assert(list_len(new) == 1)
            \\assert(list_get(new, 0) == 2)
            ,
        },
        .{
            .description = "filter with default arg",
            .source =
            \\new = list_filter([1, 2, 3], (n) -> n % 2 == 0)
            \\assert(list_len(new) == 1)
            \\assert(list_get(new, 0) == 2)
            ,
        },
        .{
            .description = "concat",
            .source =
            \\a = [1, 2, 3]
            \\b = [4, 5, 6]
            \\c = a + b
            \\assert(list_len(c) == 6)
            \\assert(list_get(c, 0) == 1)
            \\assert(list_get(c, 1) == 2)
            \\assert(list_get(c, 2) == 3)
            \\assert(list_get(c, 3) == 4)
            \\assert(list_get(c, 4) == 5)
            \\assert(list_get(c, 5) == 6)
            ,
        },
        .{
            .description = "equality",
            .source =
            \\a = [1, 2, 3]
            \\b = a
            \\c = [1, 2, 3]
            \\d = [1, 2, 3, 4]
            \\e = [5, 6, 7]
            \\assert(a == b)
            \\assert(a == c)
            \\assert(b == c)
            \\assert(a != d)
            \\assert(d != a)
            \\assert(a != e)
            \\assert(e != a)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate function tests", &test_cases, run);
}
