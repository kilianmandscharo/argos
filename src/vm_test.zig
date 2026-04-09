const std = @import("std");
const test_utils = @import("test_utils.zig");
const virtual_machine = @import("vm.zig");
const chunck = @import("chunk.zig");

const DEBUG = true;

test "vm tests" {
    const TestCase = struct {
        description: []const u8,
        source: []const u8,
    };

    const run = struct {
        fn runTest(test_case: TestCase) anyerror!void {
            const allocator = std.testing.allocator;

            var vm = try virtual_machine.VirtualMachine.init(allocator);
            defer vm.deinit();

            const result = try vm.interpret(test_case.source);
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
            \\match (a) 5 -> a = 1
            \\
            \\assert(a == 1)
            ,
        },
        .{
            .description = "match one liner false",
            .source =
            \\let a = 5
            \\
            \\match (a) 4 -> a = 1
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
            \\match true -> a = 1
            \\
            \\assert(a == 1)
            ,
        },
        .{
            .description = "match one liner no target false",
            .source =
            \\let a = 5
            \\
            \\match false -> a = 1
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
        // .{
        //     .description = "function calls no return",
        //     .source =
        //     \\let foo = fn() {
        //     \\    print("Hello, World!")
        //     \\}
        //     \\
        //     \\let result = foo()
        //     \\
        //     \\assert(result == null)
        //     ,
        // },
        // .{
        //     .description = "function calls with return",
        //     .source =
        //     \\let foo = fn(a, b) {
        //     \\    return a + b
        //     \\}
        //     \\
        //     \\let result = foo(25, 11)
        //     \\
        //     \\assert result == 36
        //     ,
        // },
        // .{
        //     .description = "nested functions",
        //     .source =
        //     \\let foo = fn() {
        //     \\    let bar = fn(a, b) {
        //     \\        return a + b
        //     \\    }
        //     \\    return bar(2, 7)
        //     \\}
        //     \\
        //     \\let result = foo()
        //     \\
        //     \\assert result == 9
        //     ,
        // },
        // .{
        //     .description = "closure",
        //     .source =
        //     \\let foo = fn() {
        //     \\    let x = 2
        //     \\    let bar = fn(a) {
        //     \\        return a + x
        //     \\    }
        //     \\    return bar
        //     \\}
        //     \\
        //     \\let func = foo()
        //     \\let result = func(10)
        //     \\
        //     \\assert result == 12
        //     ,
        // },
        // .{
        //     .description = "deep closure",
        //     .source =
        //     \\let foo = fn() {
        //     \\    let x = 1
        //     \\    let bar = fn() {
        //     \\        let y = 2
        //     \\        let baz = fn() {
        //     \\            let z = 3
        //     \\            return x + y + z
        //     \\        }
        //     \\        return baz
        //     \\    }
        //     \\    return bar
        //     \\}
        //     \\
        //     \\let bar = foo()
        //     \\let baz = bar()
        //     \\let result = baz()
        //     \\
        //     \\assert result == 6
        //     ,
        // },
        // .{
        //     .description = "string concatenation",
        //     .source =
        //     \\let foo = "foo"
        //     \\let bar = "bar"
        //     \\let result = foo + bar
        //     \\
        //     \\assert result == "foobar"
        //     ,
        // },
        // .{
        //     .description = "list index",
        //     .source =
        //     \\let foo = List{1, 2, 3, 4, 5}
        //     \\let result = foo[0] + foo[-1]
        //     \\
        //     \\assert result == 6
        //     ,
        // },
        // .{
        //     .description = "list index assignment",
        //     .source =
        //     \\let foo = List{1, 2, 3, 4, 5}
        //     \\foo[0] = 42
        //     \\
        //     \\assert foo[0] == 42
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
        //     \\assert result == 2
        //     ,
        // },
    };

    try test_utils.runTests(TestCase, "evaluate vm tests", &test_cases, run);
}
