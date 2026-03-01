const std = @import("std");

const test_utils = @import("test_utils.zig");
const runTests = test_utils.runTests;

const vm_module = @import("vm.zig");
const chunck_module = @import("chunk.zig");

const VirtualMachine = vm_module.VirtualMachine;
const Chunk = chunck_module.Chunk;

const DEBUG = true;

test "vm tests" {
    const TestCase = struct {
        description: []const u8,
        source: []const u8,
    };

    const run = struct {
        fn runTest(test_case: TestCase) anyerror!void {
            const allocator = std.testing.allocator;

            var vm = VirtualMachine.init(allocator);
            defer vm.deinit();

            var chunk = Chunk.init();
            defer chunk.deinit(allocator);

            const result = try vm.interpret(&chunk, test_case.source);
            std.debug.print("{}\n", .{result});
        }
    }.runTest;

    const test_cases = [_]TestCase{
        .{
            .description = "global variable delcaration",
            .source =
            \\let a = 6
            \\let b = 7
            \\let c = a + b
            \\assert c == 13
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
            \\assert c == 6
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
            \\assert a == 13
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
            \\assert a == 11
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
            \\assert a == 20
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
            \\assert a == 17
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
            \\assert a == 6
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
            \\assert a == 6
            \\assert d == 27
            \\assert e == 12
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
            \\assert b == 1
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
            \\assert b == 2
            ,
        },
        .{
            .description = "match third branch",
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
            \\    else -> {
            \\        b = 3
            \\    }
            \\}
            \\
            \\assert b == 3
            ,
        },
    };

    try runTests(TestCase, "evaluate vm tests", &test_cases, run);
}
