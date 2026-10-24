const std = @import("std");
const test_utils = @import("test_utils.zig");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");

test "tail recursion" {
    const TestCase = struct {
        description: []const u8,
        source: []const u8,
        expected: bool,
    };

    const run = struct {
        fn runTest(test_case: TestCase) anyerror!void {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            var script_context = vm.ScriptContext{
                .file_name = "",
                .source = test_case.source,
                .lines = .empty,
            };

            const ast = try parser.createAst(arena.allocator(), &script_context);
            const function = &ast.data.Function.body.data.Grouping.items[0].data.Declaration.right.data.Function;
            const result = compiler.Compiler.isTailRecursive(function);
            try std.testing.expectEqual(test_case.expected, result);
        }
    }.runTest;

    const test_cases = [_]TestCase{
        .{
            .description = "simple function",
            .source =
            \\foo = () -> "foo"
            ,
            .expected = true,
        },
        .{
            .description = "factorial non-tail",
            .source =
            \\fac = (n) -> if n <= 1 then 1 else n * fac(n-1)
            ,
            .expected = false,
        },
        .{
            .description = "factorial tail",
            .source =
            \\fac = (n, acc) -> if n <= 1 then acc else fac(n-1, acc*n)
            ,
            .expected = true,
        },
    };

    try test_utils.runTests(TestCase, "evaluate tail recursion tests", &test_cases, run);
}
