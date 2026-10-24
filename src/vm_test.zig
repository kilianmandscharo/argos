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

        var vm = try virtual_machine.VirtualMachine.init(std.testing.io, std.testing.allocator, arena.allocator());
        defer vm.deinit();

        const result = try vm.interpret("test", test_case.source);
        try std.testing.expect(result == .Ok);

        if (vm.stack_top != 0) {
            std.debug.print("Stack not empty at end of program! {d} values remaining\n", .{vm.stack_top});
            for (0..vm.stack_top) |i| {
                std.debug.print("  [{d}] {f}\n", .{ i, vm.stack[i] });
            }
            return error.StackNotEmpty;
        }
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
            \\@assert(c == 13)
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
            \\    @assert(d == 14)
            \\)
            \\@assert(c == 13)
            ,
        },
        .{
            .description = "using grouping in declaration",
            .source =
            \\a = (
            \\    b = 1
            \\    b + 1
            \\)
            \\@assert(a == 2)
            ,
        },
        .{
            .description = "using grouping in binary expression and declaration",
            .source =
            \\a = (
            \\    b = 1
            \\    b + 1
            \\) + 2
            \\@assert(a == 4)
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
            \\@assert(a == 5)
            ,
        },
        .{
            .description = "logical and true",
            .source =
            \\a = true and true
            \\
            \\@assert(a == true)
            ,
        },
        .{
            .description = "logical and false",
            .source =
            \\a = false and true
            \\
            \\@assert(a == false)
            ,
        },
        .{
            .description = "logical and false on right hand side",
            .source =
            \\a = true and false
            \\
            \\@assert(a == false)
            ,
        },
        .{
            .description = "logical or false",
            .source =
            \\a = false or false
            \\
            \\@assert(a == false)
            ,
        },
        .{
            .description = "logical or true both",
            .source =
            \\a = true or true
            \\
            \\@assert(a == true)
            ,
        },
        .{
            .description = "logical or true left",
            .source =
            \\a = true or false
            \\
            \\@assert(a == true)
            ,
        },
        .{
            .description = "logical or true right",
            .source =
            \\a = false or true
            \\
            \\@assert(a == true)
            ,
        },
        .{
            .description = "string concatenation",
            .source =
            \\foo = "foo"
            \\bar = "bar"
            \\result = foo + bar
            \\
            \\@assert(result == "foobar")
            ,
        },
        .{
            .description = "binary and unary",
            .source =
            \\@assert((5 + 5) == 10)
            \\@assert((10 - 20) == -10)
            \\@assert((10 * 54) == 540)
            \\@assert((2.5 * 10) == 25)
            \\@assert((20 * 4.5) == 90)
            \\@assert((10 / 5) == 2)
            \\@assert((10 / 2.5) == 4)
            \\@assert((20.5 / 2) == 10.25)
            \\@assert((100 % 30) == 10)
            \\@assert((30 % 100) == 30)
            \\@assert((1 & 1) == 1)
            \\@assert((1 & 0) == 0)
            \\@assert((1 | 1) == 1)
            \\@assert((1 | 0) == 1)
            \\@assert((1 ^ 1) == 0)
            \\@assert((0 ^ 1) == 1)
            \\@assert((2 << 2) == 8)
            \\@assert((16 >> 2) == 4)
            \\@assert((~10) == -11)
            \\@assert((5.5 % 3.1) == 2.4)
            \\@assert((5 > 3) == true)
            \\@assert((4 < 10) == true)
            \\@assert((3 <= 3) == true)
            \\@assert((2 <= 3) == true)
            \\@assert((2 >= 2) == true)
            \\@assert((2 >= 1) == true)
            ,
        },
        .{
            .description = "precedence",
            .source =
            \\@assert(2 + 3 * 4 == 14)
            \\@assert(2 * 3 + 4 == 10)
            \\@assert(2 * (3 + 4) == 14)
            \\@assert(10 - 3 - 2 == 5)
            \\@assert(2 + 3 * 4 - 1 == 13)
            \\@assert(10 / 2 + 3 == 8)
            \\@assert(10 / (2 + 3) == 2.0)
            \\@assert(2 * 3 * 4 == 24)
            \\@assert(true and false or true == true)
            \\@assert(true or false and false == true)
            \\@assert(!false and true == true)
            \\@assert(1 | 2 | 4 == 7)
            \\@assert(7 & 6 & 4 == 4)
            \\@assert(1 << 1 << 1 == 4)
            \\@assert(16 >> 1 >> 1 == 4)
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
            \\@assert(bar == 42)
            ,
        },
        .{
            .description = "evaluate to else",
            .source =
            \\foo = false
            \\bar = if foo then 42 else 43
            \\@assert(bar == 43)
            ,
        },
        .{
            .description = "evaluate to elif",
            .source =
            \\foo = 43
            \\bar = if foo == 42 then 1
            \\      elif foo == 43 then 2
            \\      else 3
            \\@assert(bar == 2)
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
            \\@assert(bar == 1)
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
            \\@assert(bar == 5)
            ,
        },
        .{
            .description = "implicit else",
            .source =
            \\foo = 41
            \\bar = if foo == 42 then @assert(true)
            \\@assert(bar == ())
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
            \\@assert(foo == true)
            ,
        },
        .{
            .description = "second branch",
            .source =
            \\foo = match 42 with 
            \\| 41 -> 1
            \\| 42 -> 2
            \\@assert(foo == 2)
            ,
        },
        .{
            .description = "wildcard at end",
            .source =
            \\foo = match 43 with 
            \\| 41 -> 1
            \\| 42 -> 2
            \\| _ -> 3
            \\@assert(foo == 3)
            ,
        },
        .{
            .description = "wildcard at start",
            .source =
            \\foo = match 42 with 
            \\| _ -> 3
            \\| 42 -> 1
            \\| 43 -> 2
            \\@assert(foo == 3)
            ,
        },
        .{
            .description = "wildcard in middle",
            .source =
            \\foo = match 42 with 
            \\| 43 -> 1
            \\| _ -> 3
            \\| 42 -> 2
            \\@assert(foo == 3)
            ,
        },
        .{
            .description = "fallthrough",
            .source =
            \\foo = match 41 with 
            \\| 42 -> @print("foo")
            \\| 43 -> @print("bar")
            \\@assert(foo == ())
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
            \\@assert(bar == 3)
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
            \\@assert(bar == 5)
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
            \\@assert(foo == 3)
            ,
        },
        .{
            .description = "match on empty list",
            .source =
            \\list = []
            \\foo = match list with 
            \\| 42 -> 1
            \\| [] -> 2
            \\| 43 -> 3
            \\@assert(foo == 2)
            ,
        },
        .{
            .description = "match on cons pattern",
            .source =
            \\list = [1, 2, 3]
            \\foo = match list with 
            \\| [] -> false
            \\| h :: t -> h == 1 and @len(t) == 2
            \\@assert(foo == true)
            \\@assert(@len(list) == 3)
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
            \\@assert(result == 6)
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
            \\foo = () -> @print("foo")
            \\result = foo()
            \\@assert(result == ())
            ,
        },
        .{
            .description = "number return",
            .source =
            \\foo = (a, b) -> a + b
            \\result = foo(33, 32)
            \\@assert(result == 65)
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
            \\@assert(result == 49)
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
            \\@assert(result == 6)
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
            \\@assert(result == 20)
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
            \\@assert(result == 6)
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
            \\@assert(result == 12)
            ,
        },
        .{
            .description = "iife",
            .source =
            \\foo = ((a, b) -> a + b)(1, 2)
            \\@assert(foo == 3)
            ,
        },
        .{
            .description = "recursion",
            .source =
            \\fac = (n) -> if n <= 1 then 1 else n * fac(n-1)
            \\result = fac(5)
            \\@assert(result == 120)
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
            \\@assert(result == 120)
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
            \\@assert(result == 5)
            ,
        },
        .{
            .description = "tail recursion",
            .source =
            \\count_up = (n, acc) -> if n <= 0 then acc else count_up(n-1, acc+1)
            \\result = count_up(5000, 0)
            \\@assert(result == 5000)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate function tests", &test_cases, run);
}

test "list tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "empty list",
            .source =
            \\foo = []
            \\l = @len(foo)
            \\@assert(l == 0)
            ,
        },
        .{
            .description = "with items",
            .source =
            \\foo = [1, 2, 3]
            \\@assert(@nth(foo, 0) == 1)
            \\@assert(@nth(foo, 1) == 2)
            \\@assert(@nth(foo, 2) == 3)
            \\@assert(@len(foo) == 3)
            ,
        },
        .{
            .description = "cons with empty list",
            .source =
            \\foo = 1 :: []
            \\@assert(@nth(foo, 0) == 1)
            \\@assert(@len(foo) == 1)
            ,
        },
        .{
            .description = "cons with filled list",
            .source =
            \\foo = 1 :: [2]
            \\@assert(@nth(foo, 0) == 1)
            \\@assert(@nth(foo, 1) == 2)
            \\@assert(@len(foo) == 2)
            ,
        },
        .{
            .description = "nested cons",
            .source =
            \\foo = 1 :: (2 :: [])
            \\@assert(@len(foo) == 2)
            \\@assert(@nth(foo, 0) == 1)
            \\@assert(@nth(foo, 1) == 2)
            ,
        },
        .{
            .description = "immutable list",
            .source =
            \\foo = [1]
            \\bar = 2 :: foo
            \\
            \\@assert(@len(foo) == 1)
            \\@assert(@nth(foo, 0) == 1)
            \\
            \\@assert(@len(bar) == 2)
            \\@assert(@nth(bar, 0) == 2)
            \\@assert(@nth(bar, 1) == 1)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate function tests", &test_cases, run);
}
