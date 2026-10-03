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
            .description = "global variable delcaration",
            .source =
            \\let a = 6
            \\let b = 7
            \\let c = a + b
            \\assert(c == 13)
            ,
        },
        .{
            .description = "global variable assignment",
            .source =
            \\let a = 6
            \\let b = 7
            \\a = 2
            \\b = 4
            \\let c = a + b
            \\assert(c == 6)
            ,
        },
        .{
            .description = "plus assign",
            .source =
            \\let a = 6
            \\a += 12
            \\assert(a == 18)
            ,
        },
        .{
            .description = "minus assign",
            .source =
            \\let a = 6
            \\a -= 12
            \\assert(a == -6)
            ,
        },
        .{
            .description = "multiply assign",
            .source =
            \\let a = 6
            \\a *= 12
            \\assert(a == 72)
            ,
        },
        .{
            .description = "divide assign",
            .source =
            \\let a = 6
            \\a /= 12
            \\assert(a == 0.5)
            ,
        },
        .{
            .description = "modulo assign",
            .source =
            \\let a = 6
            \\a %= 4
            \\assert(a == 2)
            ,
        },
        .{
            .description = "bitwise and assign",
            .source =
            \\let a = 1
            \\a &= 0
            \\assert(a == 0)
            ,
        },
        .{
            .description = "bitwise or assign",
            .source =
            \\let a = 1
            \\a |= 0
            \\assert(a == 1)
            ,
        },
        .{
            .description = "bitwise xor assign",
            .source =
            \\let a = 1
            \\a ^= 1
            \\assert(a == 0)
            ,
        },
        .{
            .description = "bitwise left shif assign",
            .source =
            \\let a = 1
            \\a <<= 3
            \\assert(a == 8)
            ,
        },
        .{
            .description = "bitwise right shif assign",
            .source =
            \\let a = 16
            \\a >>= 2
            \\assert(a == 4)
            ,
        },
        .{
            .description = "local variable declaration",
            .source =
            \\let a = 6
            \\
            \\{
            \\    let b = 7
            \\    a = a + b
            \\}
            \\
            \\assert(a == 13)
            ,
        },
        .{
            .description = "local variable assignment",
            .source =
            \\let a = 6
            \\
            \\{
            \\    let b = 7
            \\    b = 5
            \\    a = a + b
            \\}
            \\
            \\assert(a == 11)
            ,
        },
        .{
            .description = "local variable declaration nested",
            .source =
            \\let a = 6
            \\
            \\{
            \\    let b = 7
            \\    a = a + b
            \\
            \\    {
            \\        let c = 7
            \\        a = a + c
            \\    }
            \\}
            \\
            \\assert(a == 20)
            ,
        },
        .{
            .description = "local variable assignment nested",
            .source =
            \\let a = 6
            \\
            \\{
            \\    let b = 7
            \\    a = a + b
            \\
            \\    {
            \\        let c = 7
            \\        c = 2
            \\        b = 2
            \\        a = a + b + c
            \\    }
            \\}
            \\
            \\assert(a == 17)
            ,
        },
        .{
            .description = "shadowing",
            .source =
            \\let a = 6
            \\
            \\{
            \\    let a = 10
            \\    let b = 10
            \\    a = a + b
            \\}
            \\
            \\assert(a == 6)
            ,
        },
        .{
            .description = "shadowing nested",
            .source =
            \\let a = 6
            \\let d = 6
            \\let e = 6
            \\
            \\{
            \\    let a = 10
            \\    let b = 10
            \\    a = 9
            \\    b = 9
            \\
            \\    {
            \\        let a = 3
            \\        let b = 3
            \\        let c = 3
            \\        a = 2
            \\        b = 2
            \\        c = 2
            \\        a = a + b + c
            \\        e = e + a
            \\    }
            \\
            \\    a = a + b
            \\    d = a + b
            \\}
            \\
            \\assert(a == 6)
            \\assert(d == 27)
            \\assert(e == 12)
            ,
        },
        .{
            .description = "match as expression",
            .source =
            \\let a = 5
            \\
            \\let b = match (a) 5 -> 10
            \\
            \\assert(b == 10)
            ,
        },
        .{
            .description = "match as expression null",
            .source =
            \\let a = 5
            \\
            \\let b = match (a) 4 -> 10
            \\
            \\assert(b == null)
            ,
        },
        .{
            .description = "match one liner true",
            .source =
            \\let a = 5
            \\
            \\match (a) 5 -> { a = 1 }
            \\
            \\assert(a == 1)
            ,
        },
        .{
            .description = "match one liner false",
            .source =
            \\let a = 5
            \\
            \\match (a) 4 -> { a = 1 }
            \\
            \\assert(a == 5)
            ,
        },
        .{
            .description = "match first branch",
            .source =
            \\let a = true
            \\let b
            \\
            \\match (a) {
            \\    true -> {
            \\        b = 1
            \\    }
            \\    false -> {
            \\        b = 2
            \\    }
            \\    else -> {
            \\        b = 3
            \\    }
            \\}
            \\
            \\assert(b == 1)
            ,
        },
        .{
            .description = "match second branch",
            .source =
            \\let a = false
            \\let b
            \\
            \\match (a) {
            \\    true -> {
            \\        b = 1
            \\    }
            \\    false -> {
            \\        b = 2
            \\    }
            \\    else -> {
            \\        b = 3
            \\    }
            \\}
            \\
            \\assert(b == 2)
            ,
        },
        .{
            .description = "match else branch",
            .source =
            \\let a = "foo"
            \\let b
            \\
            \\match (a) {
            \\    true -> {
            \\        b = 1
            \\    }
            \\    false -> {
            \\        b = 2
            \\    }
            \\    _ -> {
            \\        b = 3
            \\    }
            \\}
            \\
            \\assert(b == 3)
            ,
        },
        .{
            .description = "no match",
            .source =
            \\let a = "foo"
            \\let b
            \\
            \\match (a) {
            \\    true -> {
            \\        b = 1
            \\    }
            \\    false -> {
            \\        b = 2
            \\    }
            \\}
            \\
            \\assert(b == null)
            ,
        },
        .{
            .description = "multi line with assignment first branch",
            .source =
            \\let a = match {
            \\    true -> 1
            \\    false -> 2
            \\}
            \\
            \\assert(a == 1)
            ,
        },
        .{
            .description = "multi line with assignment second branch",
            .source =
            \\let a = match {
            \\    false -> 1
            \\    true -> 2
            \\}
            \\
            \\assert(a == 2)
            ,
        },
        .{
            .description = "multi line with assignment else branch",
            .source =
            \\let a = match {
            \\    5 < 3 -> 1
            \\    3 > 5 -> 2
            \\    _ -> 3
            \\}
            \\
            \\assert(a == 3)
            ,
        },
        .{
            .description = "multi line with assignment no match",
            .source =
            \\let a = match {
            \\    5 < 3 -> 1
            \\    3 > 5 -> 2
            \\}
            \\
            \\assert(a == null)
            ,
        },
        .{
            .description = "match one liner no target true",
            .source =
            \\let a = 5
            \\
            \\match true -> { a = 1 }
            \\
            \\assert(a == 1)
            ,
        },
        .{
            .description = "match one liner no target false",
            .source =
            \\let a = 5
            \\
            \\match false -> { a = 1 }
            \\
            \\assert(a == 5)
            ,
        },
        .{
            .description = "match first branch no target",
            .source =
            \\let a = 5
            \\
            \\match {
            \\    5 < 10 -> {
            \\        a = 1
            \\    }
            \\    3 < 11 -> {
            \\        a = 2
            \\    }
            \\    _ -> {
            \\        a = 3
            \\    }
            \\}
            \\
            \\assert(a == 1)
            ,
        },
        .{
            .description = "match second branch no target",
            .source =
            \\let a = 5
            \\
            \\match {
            \\    5 > 10 -> {
            \\        a = 1
            \\    }
            \\    3 < 11 -> {
            \\        a = 2
            \\    }
            \\    else -> {
            \\        a = 3
            \\    }
            \\}
            \\
            \\assert(a == 2)
            ,
        },
        .{
            .description = "match else branch no target",
            .source =
            \\let a = 5
            \\
            \\match {
            \\    5 > 10 -> {
            \\        a = 1
            \\    }
            \\    3 > 11 -> {
            \\        a = 2
            \\    }
            \\    _ -> {
            \\        a = 3
            \\    }
            \\}
            \\
            \\assert(a == 3)
            ,
        },
        .{
            .description = "no match no target",
            .source =
            \\let a = 5
            \\
            \\match {
            \\    5 > 10 -> {
            \\        a = 1
            \\    }
            \\    3 > 11 -> {
            \\        a = 2
            \\    }
            \\}
            \\
            \\assert(a == 5)
            ,
        },
        .{
            .description = "match block expression",
            .source =
            \\let a = 5
            \\
            \\let b = match(a) {
            \\    5 -> {
            \\        a = 1
            \\        10 + 4
            \\    }
            \\    _ -> 3 + 3
            \\}
            \\
            \\assert(a == 1)
            \\assert(b == 14)
            ,
        },
        .{
            .description = "logical and true",
            .source =
            \\let a = true and true
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "logical and false",
            .source =
            \\let a = false and true
            \\
            \\assert(a == false)
            ,
        },
        .{
            .description = "logical and false on right hand side",
            .source =
            \\let a = true and false
            \\
            \\assert(a == false)
            ,
        },
        .{
            .description = "logical or false",
            .source =
            \\let a = false or false
            \\
            \\assert(a == false)
            ,
        },
        .{
            .description = "logical or true both",
            .source =
            \\let a = true or true
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "logical or true left",
            .source =
            \\let a = true or false
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "logical or true right",
            .source =
            \\let a = false or true
            \\
            \\assert(a == true)
            ,
        },
        .{
            .description = "string concatenation",
            .source =
            \\let foo = "foo"
            \\let bar = "bar"
            \\let result = foo + bar
            \\
            \\assert(result == "foobar")
            ,
        },
        .{
            .description = "list index",
            .source =
            \\let foo = List{1, 2, 3, 4, 5}
            \\let result = foo[0] + foo[-1]
            \\
            \\assert(result == 6)
            ,
        },
        .{
            .description = "list index assignment",
            .source =
            \\let foo = List{1, 2, 3, 4, 5}
            \\foo[0] = 42
            \\
            \\assert(foo[0] == 42)
            ,
        },
        .{
            .description = "local list index assignment",
            .source =
            \\let foo = fn(index) {
            \\    let l = List{1, 2, 3}
            \\    return l[index]
            \\}
            \\
            \\let result = foo(1)
            \\
            \\assert(result == 2)
            ,
        },
        .{
            .description = "table index",
            .source =
            \\let foo = Table{"a" = 1, "b" = 2}
            \\let result = foo["a"] + foo["b"]
            \\
            \\assert(result == 3)
            ,
        },
        .{
            .description = "table index assignment",
            .source =
            \\let foo = Table{"a" = 1, "b" = 2}
            \\foo["a"] = 10
            \\
            \\assert(foo["a"] == 10)
            ,
        },
        .{
            .description = "local table index assignment",
            .source =
            \\let foo = fn(key) {
            \\    let t = Table{"a" = 10, "b" = 11}
            \\    return t[key]
            \\}
            \\
            \\let result = foo("b")
            \\
            \\assert(result == 11)
            ,
        },
        .{
            .description = "var as table key",
            .source =
            \\let first = "a"
            \\let second = "b"
            \\
            \\let foo = Table{first = 10, second = 11}
            \\let result = foo[first] + foo["b"]
            \\
            \\assert(result == 21)
            ,
        },
        .{
            .description = "infix and prefix",
            .source =
            \\assert((5 + 5) == 10)
            \\assert((10 - 20) == -10)
            \\assert((10 * 54) == 540)
            \\assert((2.5 * 10) == 25)
            \\assert((20 * 4.5) == 90)
            \\assert((10 / 5) == 2)
            \\assert((10 / 2.5) == 4)
            \\assert((20.5 / 2) == 10.25)
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
            \\assert(10 / (2 + 3) == 2.0)
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
        .{
            .description = "addition after assignment",
            .source =
            \\let a = 5
            \\let b = 5
            \\a = "foo"
            \\b = "bar"
            \\assert(a + b == "foobar")
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate vm tests", &test_cases, run);
}

test "function tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "function calls no return",
            .source =
            \\let foo = fn() {
            \\    print("Hello, World!")
            \\}
            \\
            \\let result = foo()
            \\
            \\assert(result == null)
            ,
        },
        .{
            .description = "function calls with return",
            .source =
            \\let foo = fn(a, b) {
            \\    return a + b
            \\}
            \\
            \\let result = foo(25, 11)
            \\
            \\assert(result == 36)
            ,
        },
        .{
            .description = "nested functions",
            .source =
            \\let foo = fn() {
            \\    let bar = fn(a, b) {
            \\        return a + b
            \\    }
            \\    return bar(2, 7)
            \\}
            \\
            \\let result = foo()
            \\
            \\assert(result == 9)
            ,
        },
        .{
            .description = "closure",
            .source =
            \\let foo = fn() {
            \\    let x = 2
            \\    let bar = fn(a) {
            \\        return a + x
            \\    }
            \\    return bar
            \\}
            \\
            \\let func = foo()
            \\let result = func(10)
            \\
            \\assert(result == 12)
            ,
        },
        .{
            .description = "deep closure",
            .source =
            \\let foo = fn() {
            \\    let x = 1
            \\    let bar = fn() {
            \\        let y = 2
            \\        let baz = fn() {
            \\            let z = 3
            \\            return x + y + z
            \\        }
            \\        return baz
            \\    }
            \\    return bar
            \\}
            \\
            \\let bar = foo()
            \\let baz = bar()
            \\let result = baz()
            \\
            \\assert(result == 6)
            ,
        },
        .{
            .description = "one line expression",
            .source =
            \\let foo = fn(a, b) a + b
            \\
            \\assert(foo(1, 14) == 15)
            ,
        },
        .{
            .description = "implicit return",
            .source =
            \\let foo = fn(a, b) {
            \\    a + b
            \\}
            \\
            \\assert(foo(1, 14) == 15)
            ,
        },
        .{
            .description = "implicit return with clean up",
            .source =
            \\let foo = fn(a, b) {
            \\    let c = 5
            \\    let d = 6
            \\    a + b + c + d
            \\}
            \\
            \\assert(foo(1, 14) == 26)
            ,
        },
        .{
            .description = "match as return",
            .source =
            \\let max = fn(a, b) {
            \\    match {
            \\        a < b -> b
            \\        _ -> a
            \\    }
            \\}
            \\
            \\assert(max(1, 14) == 14)
            \\assert(max(12, 7) == 12)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate function tests", &test_cases, run);
}

test "loop tests" {
    const test_cases = [_]TestCase{
        .{
            .description = "while loop",
            .source =
            \\let a = 10
            \\let b = 0
            \\
            \\while (a > 0) {
            \\    a = a - 1
            \\    b = b + 1
            \\}
            \\
            \\assert(a == 0)
            \\assert(b == 10)
            ,
        },
        .{
            .description = "while one liner",
            .source =
            \\let a = 10
            \\
            \\while (a < 20) a = a + 1
            \\
            \\assert(a == 20)
            ,
        },
        .{
            .description = "for loop",
            .source =
            \\let a = 0
            \\let b = 0
            \\
            \\for (0..3) |i| {
            \\    a = a + i
            \\    let c = 1
            \\    b = b + c
            \\}
            \\
            \\assert(a == 3)
            \\assert(b == 3)
            ,
        },
        .{
            .description = "for loop with vars as range",
            .source =
            \\let a = 0
            \\let start = 0
            \\let end = 3
            \\
            \\for (start..end) |i| {
            \\    a = a + i
            \\}
            \\
            \\assert(a == 3)
            ,
        },
        .{
            .description = "for loop one liner",
            .source =
            \\let a = 0
            \\let start = 0
            \\let end = 3
            \\
            \\for (start..end) |i| a = a + 1
            \\
            \\assert(a == 3)
            ,
        },
    };

    try test_utils.runTests(TestCase, "evaluate loop tests", &test_cases, run);
}
