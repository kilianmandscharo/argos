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

fn expectType(expected: compiler.Type, actual: compiler.Type) !void {
    try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));

    switch (expected) {
        .Var => |v| {
            try std.testing.expectEqual(v.id, actual.Var.id);
            if (v.link) |link| {
                try expectType(link.*, actual.Var.link.?.*);
            } else {
                try std.testing.expectEqual(actual.Var.link, null);
            }
        },
        .Const => |c| {
            try std.testing.expectEqual(c, actual.Const);
        },
        .Func => |f| {
            try std.testing.expectEqual(f.from.len, actual.Func.from.len);
            for (0..f.from.len) |i| {
                try expectType(f.from[i].*, actual.Func.from[i].*);
            }
            try expectType(f.to.*, actual.Func.to.*);
        },
        .List => |l| {
            try expectType(l.*, actual.List.*);
        },
    }
}

fn newVar(arena: std.mem.Allocator, id: u32) !*compiler.Var {
    const v = try arena.create(compiler.Var);
    v.* = .{ .id = id, .link = null };
    return v;
}

test "type checker" {
    const TestCase = struct {
        description: []const u8,
        source: []const u8,
        expected: compiler.Type,
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
            const expression = ast.data.Function.body;
            var checker = compiler.TypeChecker.init(arena.allocator());
            const result = try checker.checkTypes(expression);
            try expectType(test_case.expected, result.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const c = compiler.TypeChecker.c;

    const test_cases = [_]TestCase{
        .{
            .description = "lambda without params",
            .source =
            \\() -> "foo"
            ,
            .expected = .{ .Func = .{ .from = &.{}, .to = c(.String) } },
        },
        .{
            .description = "identity lambda",
            .source =
            \\(x) -> x
            ,
            .expected = .{
                .Func = .{
                    .from = &.{&.{ .Var = try newVar(arena.allocator(), 0) }},
                    .to = &.{ .Var = try newVar(arena.allocator(), 0) },
                },
            },
        },
        .{
            .description = "lambda with binary return",
            .source =
            \\() -> 1 + 2
            ,
            .expected = .{
                .Func = .{
                    .from = &.{},
                    .to = c(.Integer),
                },
            },
        },
        .{
            .description = "lambda with binary return (from vars)",
            .source =
            \\() -> (
            \\    a = 1
            \\    b = 1
            \\    a + b
            \\)
            ,
            .expected = .{
                .Func = .{
                    .from = &.{},
                    .to = c(.Integer),
                },
            },
        },
        .{
            .description = "lambda with binary return (from vars)",
            .source =
            \\() -> (
            \\    a = 1
            \\    b = 1
            \\    a + b
            \\)
            ,
            .expected = .{
                .Func = .{
                    .from = &.{},
                    .to = c(.Integer),
                },
            },
        },
        .{
            .description = "function call",
            .source =
            \\add = (a, b) -> a + b
            \\add(1, 2)
            ,
            .expected = c(.Integer).*,
        },
    };

    try test_utils.runTests(TestCase, "evaluate type checker tests", &test_cases, run);
}
