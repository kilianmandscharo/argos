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
            .description = "global variable assignment",
            .input =
            \\let a = 6
            \\let b = 7
            \\let c = a + b
            ,
        },
    };

    try runTests(TestCase, "evaluate memory leaks", &test_cases, run);
}
