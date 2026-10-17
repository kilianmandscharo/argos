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
        // .{
        //     .description = "match as expression",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\let b = match (a) 5 -> 10
        //     \\
        //     \\assert(b == 10)
        //     ,
        // },
        // .{
        //     .description = "match as expression null",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\let b = match (a) 4 -> 10
        //     \\
        //     \\assert(b == null)
        //     ,
        // },
        // .{
        //     .description = "match one liner true",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match (a) 5 -> { a = 1 }
        //     \\
        //     \\assert(a == 1)
        //     ,
        // },
        // .{
        //     .description = "match one liner false",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match (a) 4 -> { a = 1 }
        //     \\
        //     \\assert(a == 5)
        //     ,
        // },
        // .{
        //     .description = "match first branch",
        //     .source =
        //     \\let a = true
        //     \\let b
        //     \\
        //     \\match (a) {
        //     \\    true -> {
        //     \\        b = 1
        //     \\    }
        //     \\    false -> {
        //     \\        b = 2
        //     \\    }
        //     \\    else -> {
        //     \\        b = 3
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(b == 1)
        //     ,
        // },
        // .{
        //     .description = "match second branch",
        //     .source =
        //     \\let a = false
        //     \\let b
        //     \\
        //     \\match (a) {
        //     \\    true -> {
        //     \\        b = 1
        //     \\    }
        //     \\    false -> {
        //     \\        b = 2
        //     \\    }
        //     \\    else -> {
        //     \\        b = 3
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(b == 2)
        //     ,
        // },
        // .{
        //     .description = "match else branch",
        //     .source =
        //     \\let a = "foo"
        //     \\let b
        //     \\
        //     \\match (a) {
        //     \\    true -> {
        //     \\        b = 1
        //     \\    }
        //     \\    false -> {
        //     \\        b = 2
        //     \\    }
        //     \\    _ -> {
        //     \\        b = 3
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(b == 3)
        //     ,
        // },
        // .{
        //     .description = "no match",
        //     .source =
        //     \\let a = "foo"
        //     \\let b
        //     \\
        //     \\match (a) {
        //     \\    true -> {
        //     \\        b = 1
        //     \\    }
        //     \\    false -> {
        //     \\        b = 2
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(b == null)
        //     ,
        // },
        // .{
        //     .description = "multi line with assignment first branch",
        //     .source =
        //     \\let a = match {
        //     \\    true -> 1
        //     \\    false -> 2
        //     \\}
        //     \\
        //     \\assert(a == 1)
        //     ,
        // },
        // .{
        //     .description = "multi line with assignment second branch",
        //     .source =
        //     \\let a = match {
        //     \\    false -> 1
        //     \\    true -> 2
        //     \\}
        //     \\
        //     \\assert(a == 2)
        //     ,
        // },
        // .{
        //     .description = "multi line with assignment else branch",
        //     .source =
        //     \\let a = match {
        //     \\    5 < 3 -> 1
        //     \\    3 > 5 -> 2
        //     \\    _ -> 3
        //     \\}
        //     \\
        //     \\assert(a == 3)
        //     ,
        // },
        // .{
        //     .description = "multi line with assignment no match",
        //     .source =
        //     \\let a = match {
        //     \\    5 < 3 -> 1
        //     \\    3 > 5 -> 2
        //     \\}
        //     \\
        //     \\assert(a == null)
        //     ,
        // },
        // .{
        //     .description = "match one liner no target true",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match true -> { a = 1 }
        //     \\
        //     \\assert(a == 1)
        //     ,
        // },
        // .{
        //     .description = "match one liner no target false",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match false -> { a = 1 }
        //     \\
        //     \\assert(a == 5)
        //     ,
        // },
        // .{
        //     .description = "match first branch no target",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match {
        //     \\    5 < 10 -> {
        //     \\        a = 1
        //     \\    }
        //     \\    3 < 11 -> {
        //     \\        a = 2
        //     \\    }
        //     \\    _ -> {
        //     \\        a = 3
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(a == 1)
        //     ,
        // },
        // .{
        //     .description = "match second branch no target",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match {
        //     \\    5 > 10 -> {
        //     \\        a = 1
        //     \\    }
        //     \\    3 < 11 -> {
        //     \\        a = 2
        //     \\    }
        //     \\    else -> {
        //     \\        a = 3
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(a == 2)
        //     ,
        // },
        // .{
        //     .description = "match else branch no target",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match {
        //     \\    5 > 10 -> {
        //     \\        a = 1
        //     \\    }
        //     \\    3 > 11 -> {
        //     \\        a = 2
        //     \\    }
        //     \\    _ -> {
        //     \\        a = 3
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(a == 3)
        //     ,
        // },
        // .{
        //     .description = "no match no target",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\match {
        //     \\    5 > 10 -> {
        //     \\        a = 1
        //     \\    }
        //     \\    3 > 11 -> {
        //     \\        a = 2
        //     \\    }
        //     \\}
        //     \\
        //     \\assert(a == 5)
        //     ,
        // },
        // .{
        //     .description = "match block expression",
        //     .source =
        //     \\let a = 5
        //     \\
        //     \\let b = match(a) {
        //     \\    5 -> {
        //     \\        a = 1
        //     \\        10 + 4
        //     \\    }
        //     \\    _ -> 3 + 3
        //     \\}
        //     \\
        //     \\assert(a == 1)
        //     \\assert(b == 14)
        //     ,
        // },
        // .{
        //     .description = "logical and true",
        //     .source =
        //     \\let a = true and true
        //     \\
        //     \\assert(a == true)
        //     ,
        // },
        // .{
        //     .description = "logical and false",
        //     .source =
        //     \\let a = false and true
        //     \\
        //     \\assert(a == false)
        //     ,
        // },
        // .{
        //     .description = "logical and false on right hand side",
        //     .source =
        //     \\let a = true and false
        //     \\
        //     \\assert(a == false)
        //     ,
        // },
        // .{
        //     .description = "logical or false",
        //     .source =
        //     \\let a = false or false
        //     \\
        //     \\assert(a == false)
        //     ,
        // },
        // .{
        //     .description = "logical or true both",
        //     .source =
        //     \\let a = true or true
        //     \\
        //     \\assert(a == true)
        //     ,
        // },
        // .{
        //     .description = "logical or true left",
        //     .source =
        //     \\let a = true or false
        //     \\
        //     \\assert(a == true)
        //     ,
        // },
        // .{
        //     .description = "logical or true right",
        //     .source =
        //     \\let a = false or true
        //     \\
        //     \\assert(a == true)
        //     ,
        // },
        // .{
        //     .description = "string concatenation",
        //     .source =
        //     \\let foo = "foo"
        //     \\let bar = "bar"
        //     \\let result = foo + bar
        //     \\
        //     \\assert(result == "foobar")
        //     ,
        // },
        // .{
        //     .description = "list index",
        //     .source =
        //     \\let foo = List{1, 2, 3, 4, 5}
        //     \\let result = foo[0] + foo[-1]
        //     \\
        //     \\assert(result == 6)
        //     ,
        // },
        // .{
        //     .description = "list index assignment",
        //     .source =
        //     \\let foo = List{1, 2, 3, 4, 5}
        //     \\foo[0] = 42
        //     \\
        //     \\assert(foo[0] == 42)
        //     ,
        // },
        // .{
        //     .description = "local list index assignment",
        //     .source =
        //     \\let foo = fn(index) {
        //     \\    let l = List{1, 2, 3}
        //     \\    return l[index]
        //     \\}
        //     \\
        //     \\let result = foo(1)
        //     \\
        //     \\assert(result == 2)
        //     ,
        // },
        // .{
        //     .description = "table index",
        //     .source =
        //     \\let foo = Table{"a" = 1, "b" = 2}
        //     \\let result = foo["a"] + foo["b"]
        //     \\
        //     \\assert(result == 3)
        //     ,
        // },
        // .{
        //     .description = "table index assignment",
        //     .source =
        //     \\let foo = Table{"a" = 1, "b" = 2}
        //     \\foo["a"] = 10
        //     \\
        //     \\assert(foo["a"] == 10)
        //     ,
        // },
        // .{
        //     .description = "local table index assignment",
        //     .source =
        //     \\let foo = fn(key) {
        //     \\    let t = Table{"a" = 10, "b" = 11}
        //     \\    return t[key]
        //     \\}
        //     \\
        //     \\let result = foo("b")
        //     \\
        //     \\assert(result == 11)
        //     ,
        // },
        // .{
        //     .description = "var as table key",
        //     .source =
        //     \\let first = "a"
        //     \\let second = "b"
        //     \\
        //     \\let foo = Table{first = 10, second = 11}
        //     \\let result = foo[first] + foo["b"]
        //     \\
        //     \\assert(result == 21)
        //     ,
        // },
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
