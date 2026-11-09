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

fn expectType(expected_t: *const compiler.Type, actual_t: *const compiler.Type) !void {
    const expected = expected_t.prune();
    const actual = actual_t.prune();

    try std.testing.expectEqual(std.meta.activeTag(expected.*), std.meta.activeTag(actual.*));

    switch (expected.*) {
        .Var => |v| {
            try std.testing.expectEqual(v.classes, actual.Var.classes);
            if (v.link) |link| {
                try expectType(link, actual.Var.link.?);
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
                try expectType(f.from[i], actual.Func.from[i]);
            }
            try expectType(f.to, actual.Func.to);
        },
        .List => |l| {
            try expectType(l, actual.List);
        },
    }
}

fn createVar(arena: std.mem.Allocator, classes: std.EnumSet(compiler.Class)) !*compiler.Var {
    const v = try arena.create(compiler.Var);
    v.* = .{ .id = 0, .link = null, .classes = classes };
    return v;
}

fn varType(arena: std.mem.Allocator, classes: std.EnumSet(compiler.Class)) !*compiler.Type {
    const v = try arena.create(compiler.Var);
    v.* = .{ .id = 0, .link = null, .classes = classes };
    const t = try arena.create(compiler.Type);
    t.* = .{ .Var = v };
    return t;
}

fn funcType(arena: std.mem.Allocator, params: []const *const compiler.Type, ret: *const compiler.Type) !*const compiler.Type {
    const f = try arena.create(compiler.Func);
    f.* = .{ .from = try arena.dupe(*const compiler.Type, params), .to = ret };
    const t = try arena.create(compiler.Type);
    t.* = .{ .Func = f };
    return t;
}

fn listType(arena: std.mem.Allocator, l_type: *const compiler.Type) !*const compiler.Type {
    const t = try arena.create(compiler.Type);
    t.* = .{ .List = l_type };
    return t;
}

fn unarySame(arena: std.mem.Allocator, classes: std.EnumSet(compiler.Class)) !*const compiler.Type {
    const v = try varType(arena, classes);
    return funcType(arena, &.{v}, v);
}

fn binarySame(arena: std.mem.Allocator, classes: std.EnumSet(compiler.Class)) !*const compiler.Type {
    const v = try varType(arena, classes);
    return funcType(arena, &.{ v, v }, v);
}

fn binaryTo(arena: std.mem.Allocator, classes: std.EnumSet(compiler.Class), ret: *const compiler.Type) !*const compiler.Type {
    const v = try varType(arena, classes);
    return funcType(arena, &.{ v, v }, ret);
}

const TypeCheckerTestCase = struct {
    description: []const u8,
    source: []const u8,
    expected: ?*const compiler.Type = null,
    expect_error: ?anyerror = null,
};

const runTypeCheckerTest = struct {
    fn runTest(test_case: TypeCheckerTestCase) anyerror!void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        var script_context = vm.ScriptContext{
            .file_name = "",
            .source = test_case.source,
            .lines = .empty,
        };

        const ast = try parser.createAst(arena.allocator(), &script_context);
        const expression = ast.data.Function.body;
        var checker = compiler.TypeChecker.init(arena.allocator(), &script_context);
        defer checker.deinit();
        const result = checker.checkTypes(expression);
        if (test_case.expect_error) |err| {
            try std.testing.expectError(err, result);
        } else {
            const unwrapped = try result;
            try expectType(test_case.expected.?, unwrapped.root);
        }
    }
}.runTest;

test "type checker" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const c = compiler.TypeChecker.c;
    const alloc = arena.allocator();

    const unary_test_cases = [_]TypeCheckerTestCase{
        .{ .description = "minus integer", .source = "-5", .expected = c(.Integer) },
        .{ .description = "minus float", .source = "-5.6", .expected = c(.Float) },
        .{ .description = "minus invalid type", .source = "-true", .expect_error = error.MissingInstance },
        .{ .description = "minus constrain", .source = "(x) -> -x", .expected = try unarySame(alloc, .initMany(&.{.Num})) },

        .{ .description = "bang", .source = "!true", .expected = c(.Boolean) },
        .{ .description = "bang invalid", .source = "!5", .expect_error = error.CantUnify },

        .{ .description = "tilde", .source = "~5", .expected = c(.Integer) },
        .{ .description = "tilde invalid", .source = "~1.2", .expect_error = error.CantUnify },
    };

    const binary_test_cases = [_]TypeCheckerTestCase{
        .{ .description = "plus integer", .source = "1 + 1", .expected = c(.Integer) },
        .{ .description = "plus float", .source = "1.0 + 1.0", .expected = c(.Float) },
        .{ .description = "plus string", .source = "\"foo\" + \"bar\"", .expected = c(.String) },
        .{ .description = "plus invalid", .source = "1 + 1.2", .expect_error = error.CantUnify },
        .{ .description = "plus constrain", .source = "(a, b) -> a + b", .expected = try binarySame(alloc, .initMany(&.{.Plus})) },

        .{ .description = "minus integer", .source = "1 - 1", .expected = c(.Integer) },
        .{ .description = "minus float", .source = "1.0 - 1.0", .expected = c(.Float) },
        .{ .description = "minus mismatch", .source = "1 - 1.2", .expect_error = error.CantUnify },
        .{ .description = "minus invalid type", .source = "\"a\" - \"b\"", .expect_error = error.MissingInstance },
        .{ .description = "minus constrain", .source = "(a, b) -> a - b", .expected = try binarySame(alloc, .initMany(&.{.Num})) },

        .{ .description = "multiply integer", .source = "2 * 3", .expected = c(.Integer) },
        .{ .description = "multiply float", .source = "2.0 * 3.0", .expected = c(.Float) },
        .{ .description = "multiply mismatch", .source = "2 * 3.0", .expect_error = error.CantUnify },
        .{ .description = "multiply invalid type", .source = "true * false", .expect_error = error.MissingInstance },
        .{ .description = "multiply constrain", .source = "(a, b) -> a * b", .expected = try binarySame(alloc, .initMany(&.{.Num})) },

        .{ .description = "divide integer", .source = "6 / 3", .expected = c(.Integer) },
        .{ .description = "divide float", .source = "6.0 / 3.0", .expected = c(.Float) },
        .{ .description = "divide mismatch", .source = "6 / 3.0", .expect_error = error.CantUnify },
        .{ .description = "divide invalid type", .source = "\"a\" / \"b\"", .expect_error = error.MissingInstance },
        .{ .description = "divide constrain", .source = "(a, b) -> a / b", .expected = try binarySame(alloc, .initMany(&.{.Num})) },

        .{ .description = "modulo integer", .source = "7 % 3", .expected = c(.Integer) },
        .{ .description = "modulo float", .source = "7.0 % 3.0", .expected = c(.Float) }, // drop if % is integer-only
        .{ .description = "modulo mismatch", .source = "7 % 3.0", .expect_error = error.CantUnify },
        .{ .description = "modulo invalid type", .source = "\"a\" % \"b\"", .expect_error = error.MissingInstance },
        .{ .description = "modulo constrain", .source = "(a, b) -> a % b", .expected = try binarySame(alloc, .initMany(&.{.Num})) },

        .{ .description = "eq integer", .source = "1 == 1", .expected = c(.Boolean) },
        .{ .description = "eq float", .source = "1.0 == 1.0", .expected = c(.Boolean) },
        .{ .description = "eq string", .source = "\"a\" == \"a\"", .expected = c(.Boolean) },
        .{ .description = "eq boolean", .source = "true == false", .expected = c(.Boolean) },
        .{ .description = "eq mismatch", .source = "1 == \"a\"", .expect_error = error.CantUnify },
        .{ .description = "eq constrain", .source = "(a, b) -> a == b", .expected = try binaryTo(alloc, .initMany(&.{.Eq}), c(.Boolean)) },

        .{ .description = "not eq integer", .source = "1 != 2", .expected = c(.Boolean) },
        .{ .description = "not eq string", .source = "\"a\" != \"b\"", .expected = c(.Boolean) },
        .{ .description = "not eq boolean", .source = "true != false", .expected = c(.Boolean) },
        .{ .description = "not eq mismatch", .source = "1 != true", .expect_error = error.CantUnify },
        .{ .description = "not eq constrain", .source = "(a, b) -> a != b", .expected = try binaryTo(alloc, .initMany(&.{.Eq}), c(.Boolean)) },

        .{ .description = "gt integer", .source = "2 > 1", .expected = c(.Boolean) },
        .{ .description = "gt float", .source = "2.0 > 1.0", .expected = c(.Boolean) },
        .{ .description = "gt mismatch", .source = "2 > 1.0", .expect_error = error.CantUnify },
        .{ .description = "gt constrain", .source = "(a, b) -> a > b", .expected = try binaryTo(alloc, .initMany(&.{.Ord}), c(.Boolean)) },

        .{ .description = "gt or eq integer", .source = "2 >= 1", .expected = c(.Boolean) },
        .{ .description = "gt or eq float", .source = "2.0 >= 1.0", .expected = c(.Boolean) },
        .{ .description = "gt or eq mismatch", .source = "2 >= 1.0", .expect_error = error.CantUnify },
        .{ .description = "gt or eq constrain", .source = "(a, b) -> a >= b", .expected = try binaryTo(alloc, .initMany(&.{.Ord}), c(.Boolean)) },

        .{ .description = "lt integer", .source = "1 < 2", .expected = c(.Boolean) },
        .{ .description = "lt float", .source = "1.0 < 2.0", .expected = c(.Boolean) },
        .{ .description = "lt mismatch", .source = "1 < 2.0", .expect_error = error.CantUnify },
        .{ .description = "lt constrain", .source = "(a, b) -> a < b", .expected = try binaryTo(alloc, .initMany(&.{.Ord}), c(.Boolean)) },

        .{ .description = "lt or eq integer", .source = "1 <= 2", .expected = c(.Boolean) },
        .{ .description = "lt or eq float", .source = "1.0 <= 2.0", .expected = c(.Boolean) },
        .{ .description = "lt or eq mismatch", .source = "1 <= 2.0", .expect_error = error.CantUnify },
        .{ .description = "lt or eq constrain", .source = "(a, b) -> a <= b", .expected = try binaryTo(alloc, .initMany(&.{.Ord}), c(.Boolean)) },

        .{ .description = "and booleans", .source = "true and false", .expected = c(.Boolean) },
        .{ .description = "and left invalid", .source = "1 and true", .expect_error = error.CantUnify },
        .{ .description = "and right invalid", .source = "true and 1", .expect_error = error.CantUnify },
        .{ .description = "and pins parameters", .source = "(a, b) -> a and b", .expected = try funcType(alloc, &.{ c(.Boolean), c(.Boolean) }, c(.Boolean)) },

        .{ .description = "or booleans", .source = "true or false", .expected = c(.Boolean) },
        .{ .description = "or left invalid", .source = "1 or true", .expect_error = error.CantUnify },
        .{ .description = "or right invalid", .source = "true or 1", .expect_error = error.CantUnify },
        .{ .description = "or pins parameters", .source = "(a, b) -> a or b", .expected = try funcType(alloc, &.{ c(.Boolean), c(.Boolean) }, c(.Boolean)) },

        .{ .description = "bit and", .source = "6 & 3", .expected = c(.Integer) },
        .{ .description = "bit and float", .source = "6.0 & 3.0", .expect_error = error.CantUnify },
        .{ .description = "bit and mismatch", .source = "6 & true", .expect_error = error.CantUnify },
        .{ .description = "bit and pins parameters", .source = "(a, b) -> a & b", .expected = try funcType(alloc, &.{ c(.Integer), c(.Integer) }, c(.Integer)) },

        .{ .description = "bit or", .source = "6 | 3", .expected = c(.Integer) },
        .{ .description = "bit or float", .source = "6.0 | 3.0", .expect_error = error.CantUnify },
        .{ .description = "bit or pins parameters", .source = "(a, b) -> a | b", .expected = try funcType(alloc, &.{ c(.Integer), c(.Integer) }, c(.Integer)) },

        .{ .description = "bit xor", .source = "6 ^ 3", .expected = c(.Integer) },
        .{ .description = "bit xor float", .source = "6.0 ^ 3.0", .expect_error = error.CantUnify },
        .{ .description = "bit xor pins parameters", .source = "(a, b) -> a ^ b", .expected = try funcType(alloc, &.{ c(.Integer), c(.Integer) }, c(.Integer)) },

        .{ .description = "left shift", .source = "1 << 3", .expected = c(.Integer) },
        .{ .description = "left shift float", .source = "1.0 << 3", .expect_error = error.CantUnify },
        .{ .description = "left shift pins parameters", .source = "(a, b) -> a << b", .expected = try funcType(alloc, &.{ c(.Integer), c(.Integer) }, c(.Integer)) },

        .{ .description = "right shift", .source = "8 >> 1", .expected = c(.Integer) },
        .{ .description = "right shift float", .source = "8.0 >> 1", .expect_error = error.CantUnify },
        .{ .description = "right shift pins parameters", .source = "(a, b) -> a >> b", .expected = try funcType(alloc, &.{ c(.Integer), c(.Integer) }, c(.Integer)) },
    };

    const function_test_cases = [_]TypeCheckerTestCase{
        .{
            .description = "lambda without params",
            .source =
            \\() -> "foo"
            ,
            .expected = try funcType(alloc, &.{}, c(.String)),
        },
        .{
            .description = "identity lambda",
            .source =
            \\(x) -> x
            ,
            .expected = try funcType(alloc, &.{try varType(alloc, .empty)}, try varType(alloc, .empty)),
        },
        .{
            .description = "lambda with binary return",
            .source =
            \\() -> 1 + 2
            ,
            .expected = try funcType(alloc, &.{}, c(.Integer)),
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
            .expected = try funcType(alloc, &.{}, c(.Integer)),
        },
        .{
            .description = "function call",
            .source =
            \\add = (a, b) -> a + b
            \\add(1, 2)
            ,
            .expected = c(.Integer),
        },
        .{
            .description = "recursion",
            .source =
            \\fact = (n) -> if n <= 1 then 1 else n * fact(n-1)
            \\fact(5)
            ,
            .expected = c(.Integer),
        },
        .{
            .description = "lambda as param",
            .source =
            \\foo = (func) -> func(1, 2)
            \\foo((a, b) -> a + b)
            ,
            .expected = c(.Integer),
        },
        .{
            .description = "lambda not generalizable",
            .source =
            \\foo = (func) -> (
            \\   func(1, 2)
            \\   func(1.0, 2.0)
            \\)
            \\foo((a, b) -> a + b)
            ,
            .expect_error = error.CantUnify,
        },
    };

    const list_test_cases = [_]TypeCheckerTestCase{
        .{ .description = "list empty", .source = "[]", .expected = try listType(alloc, try varType(alloc, .empty)) },
        .{ .description = "list integer", .source = "[1, 2]", .expected = try listType(alloc, c(.Integer)) },
        .{ .description = "list cons integer", .source = "1 :: []", .expected = try listType(alloc, c(.Integer)) },
        .{ .description = "list cons invalid", .source = "1 :: [1.2]", .expect_error = error.CantUnify },
    };

    const if_test_cases = [_]TypeCheckerTestCase{
        .{ .description = "if else integer", .source = "if true then 1 else 2", .expected = c(.Integer) },
        .{ .description = "if else mismatch", .source = "if true then 1 else \"a\"", .expect_error = error.CantUnify },
        .{ .description = "if non-boolean condition", .source = "if 1 then 1 else 2", .expect_error = error.CantUnify },
        .{ .description = "if without else", .source = "if true then 1", .expect_error = error.CantUnify },
        .{ .description = "if without else unit", .source = "if true then ()", .expected = c(.Unit) },
        .{ .description = "elif chain", .source = "if false then 1 elif true then 2 else 3", .expected = c(.Integer) },
        .{ .description = "elif mismatch", .source = "if false then 1 elif true then \"a\" else 3", .expect_error = error.CantUnify },
        .{ .description = "elif non-boolean condition", .source = "if false then 1 elif 2 then 3 else 4", .expect_error = error.CantUnify },
        .{ .description = "elif without else", .source = "if false then 1 elif true then 2", .expect_error = error.CantUnify },
        .{ .description = "if pins parameter", .source = "(a) -> if a then 1 else 2", .expected = try funcType(alloc, &.{c(.Boolean)}, c(.Integer)) },
    };

    const match_test_cases = [_]TypeCheckerTestCase{
        .{
            .description = "match integer",
            .source =
            \\match 1 with
            \\| 1 -> "foo"
            \\| 2 -> "bar"
            \\| _ -> "baz"
            ,
            .expected = c(.String),
        },
        .{
            .description = "match error target mismatch",
            .source =
            \\match 1 with
            \\| 1 -> "foo"
            \\| 2 -> "bar"
            \\| _ -> "baz"
            ,
            .expected = c(.String),
        },
        .{
            .description = "match error target mismatch",
            .source =
            \\match 1 with
            \\| 1 -> "foo"
            \\| 1.1 -> "bar"
            \\| _ -> "baz"
            ,
            .expect_error = error.CantUnify,
        },
        .{
            .description = "match error result mismatch",
            .source =
            \\match 1 with
            \\| 1 -> "foo"
            \\| 1 -> 2
            \\| _ -> "baz"
            ,
            .expect_error = error.CantUnify,
        },
        .{
            .description = "match list",
            .source =
            \\match [] with
            \\| [] -> "foo"
            \\| h :: t -> "bar"
            ,
            .expected = c(.String),
        },
        .{
            .description = "match list element type",
            .source =
            \\match [1, 2, 3] with
            \\| [] -> 1
            \\| h :: _ -> h
            ,
            .expected = c(.Integer),
        },
        .{
            .description = "match list type",
            .source =
            \\match [1, 2, 3] with
            \\| [] -> [1]
            \\| _ :: t -> t
            ,
            .expected = try listType(alloc, c(.Integer)),
        },
    };

    try test_utils.runTests(TypeCheckerTestCase, "evaluate unary checks", &unary_test_cases, runTypeCheckerTest);
    try test_utils.runTests(TypeCheckerTestCase, "evaluate binary checks", &binary_test_cases, runTypeCheckerTest);
    try test_utils.runTests(TypeCheckerTestCase, "evaluate function checks", &function_test_cases, runTypeCheckerTest);
    try test_utils.runTests(TypeCheckerTestCase, "evaluate list checks", &list_test_cases, runTypeCheckerTest);
    try test_utils.runTests(TypeCheckerTestCase, "evaluate if checks", &if_test_cases, runTypeCheckerTest);
    try test_utils.runTests(TypeCheckerTestCase, "evaluate match checks", &match_test_cases, runTypeCheckerTest);
}
