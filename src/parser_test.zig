const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const vm = @import("vm.zig");

fn e(data: ast.ExpressionData) ast.Expression {
    return .init(data, scanner.Token.dummy());
}

fn s(data: []const u8) scanner.Token {
    var token = scanner.Token.dummy();
    token.data = data;
    return token;
}

fn expectTag(expected: anytype, actual: anytype) !void {
    try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
}

fn expectExpression(expected: ast.Expression, actual: ast.Expression) anyerror!void {
    try expectExpressionData(expected.data, actual.data);
}

fn expectExpressionData(expected: ast.ExpressionData, actual: ast.ExpressionData) !void {
    try expectTag(expected, actual);

    switch (expected) {
        .Declaration => |declaration| {
            try std.testing.expectEqualStrings(declaration.left.data, actual.Declaration.left.data);
            try expectExpression(declaration.right.*, actual.Declaration.right.*);
        },
        .Grouping => |expr| {
            try std.testing.expectEqual(expr.items.len, actual.Grouping.items.len);
            for (0..expr.items.len) |i| {
                try expectExpression(expr.items[i].*, actual.Grouping.items[i].*);
            }
        },
        .Identifier => |ident| try std.testing.expectEqualStrings(ident, actual.Identifier),
        .String => |string| try std.testing.expectEqualStrings(string, actual.String),
        .Integer => |n| try std.testing.expectEqual(n, actual.Integer),
        .Float => |f| try std.testing.expectEqual(f, actual.Float),
        .Boolean => |b| try std.testing.expectEqual(b, actual.Boolean),
        .Unary => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Unary.operator);
            try expectExpression(expr.right.*, actual.Unary.right.*);
        },
        .Binary => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Binary.operator);
            try expectExpression(expr.left.*, actual.Binary.left.*);
            try expectExpression(expr.right.*, actual.Binary.right.*);
        },
        .Function => |func| {
            if (func.name) |name| {
                try std.testing.expectEqualStrings(name.data, actual.Function.name.?.data);
            } else {
                try std.testing.expect(actual.Function.name == null);
            }
            try std.testing.expectEqual(func.params.items.len, actual.Function.params.items.len);
            for (0..func.params.items.len) |i| {
                const first = func.params.items[i];
                const second = actual.Function.params.items[i];
                try expectTag(first, second);
                if (first == .Positional) {
                    try std.testing.expectEqualStrings(first.Positional.data, second.Positional.data);
                } else {
                    try std.testing.expectEqualStrings(first.Default.name.data, second.Default.name.data);
                    try expectExpression(first.Default.value.*, second.Default.value.*);
                }
            }
            try expectExpression(func.body.*, actual.Function.body.*);
        },
        .Call => |expr| {
            try expectExpression(expr.function.*, actual.Call.function.*);
            try std.testing.expectEqual(expr.args.items.len, actual.Call.args.items.len);
            for (0..expr.args.items.len) |i| {
                const first = expr.args.items[i];
                const second = actual.Call.args.items[i];
                try expectExpression(first.*, second.*);
            }
        },
        .List => |expr| {
            try std.testing.expectEqual(expr.items.len, actual.List.items.len);
            for (expr.items, 0..) |item, i| {
                try expectExpression(item.*, actual.List.items[i].*);
            }
        },
        .Match => |expr| {
            try expectExpression(expr.target.*, actual.Match.target.*);
            try std.testing.expectEqual(expr.arms.items.len, actual.Match.arms.items.len);
            for (0..expr.arms.items.len) |i| {
                const first = expr.arms.items[i];
                const second = actual.Match.arms.items[i];
                try expectExpression(first.pattern.*, second.pattern.*);
                try expectExpression(first.expression.*, second.expression.*);
            }
        },
        .If => |expr| {
            try std.testing.expectEqual(expr.arms.items.len, actual.If.arms.items.len);
            for (0..expr.arms.items.len) |i| {
                const first = expr.arms.items[i];
                const second = actual.If.arms.items[i];
                try expectExpression(first.condition.*, second.condition.*);
                try expectExpression(first.then.*, second.then.*);
            }
            if (expr.alt) |alt| {
                try expectExpression(alt.*, actual.If.alt.?.*);
            } else {
                try std.testing.expect(actual.If.alt == null);
            }
        },
        .Unit => {
            try std.testing.expect(actual == .Unit);
        },
    }
}

const ExpressionTestCase = struct {
    description: []const u8,
    input: []const u8,
    expected_expression: ?ast.ExpressionData = null,
    expect_error: bool = false,
};

fn runExpressionTest(arena: std.mem.Allocator, test_case: ExpressionTestCase) anyerror!void {
    const ctx = try arena.create(vm.ScriptContext);
    ctx.* = .{
        .file_name = "scanner_test",
        .source = test_case.input,
        .lines = .empty,
    };

    const result = parser.createAst(arena, ctx);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const program = try result;
        const expressions = program.data.Function.body.data.Grouping.items;
        try std.testing.expectEqual(expressions.len, 1);
        try expectExpressionData(test_case.expected_expression.?, expressions[0].data);
    }
}

test "expressions" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse identifier",
            .input =
            \\foo
            ,
            .expected_expression = .{ .Identifier = "foo" },
        },
        .{
            .description = "should parse string literal",
            .input =
            \\"foo"
            ,
            .expected_expression = .{ .String = "foo" },
        },
        .{
            .description = "should parse integer literal",
            .input =
            \\666
            ,
            .expected_expression = .{ .Integer = 666 },
        },
        .{
            .description = "should parse float literal",
            .input =
            \\3.1415
            ,
            .expected_expression = .{ .Float = 3.1415 },
        },
        .{
            .description = "should parse true",
            .input =
            \\true
            ,
            .expected_expression = .{ .Boolean = true },
        },
        .{
            .description = "should parse false",
            .input =
            \\false
            ,
            .expected_expression = .{ .Boolean = false },
        },
        .{
            .description = "should parse unit",
            .input =
            \\()
            ,
            .expected_expression = .{ .Unit = {} },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse expressions",
        &test_cases,
        runExpressionTest,
    );
}

test "unary expressions" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse bang operator with true",
            .input = "!true",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Boolean = true }),
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse bang operator with false",
            .input = "!false",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Boolean = false }),
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse tilde operator with false",
            .input = "~false",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Boolean = false }),
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with integer",
            .input = "+5",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Integer = 5 }),
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with integer",
            .input = "-2",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Integer = 2 }),
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse tilde operator with integer",
            .input = "~5",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Integer = 5 }),
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with float",
            .input = "+5.41",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Float = 5.41 }),
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with float",
            .input = "-2.1234",
            .expected_expression = .{
                .Unary = .{
                    .right = &e(.{ .Float = 2.1234 }),
                    .operator = .Minus,
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse unary expression",
        &test_cases,
        runExpressionTest,
    );
}

test "binary expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse integer addition",
            .input = "1 + 1",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 1 }),
                    .right = &e(.{ .Integer = 1 }),
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse float addition",
            .input = "1.1 + 1.35",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Float = 1.1 }),
                    .right = &e(.{ .Float = 1.35 }),
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse integer subtraction",
            .input = "40 - 22",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 40 }),
                    .right = &e(.{ .Integer = 22 }),
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse float subtraction",
            .input = "40.54 - 22.33",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Float = 40.54 }),
                    .right = &e(.{ .Float = 22.33 }),
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse integer multiplication",
            .input = "5 * 66",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 5 }),
                    .right = &e(.{ .Integer = 66 }),
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse integer mod",
            .input = "33 % 2",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 33 }),
                    .right = &e(.{ .Integer = 2 }),
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should preserve order of operations without parens",
            .input = "3 * 4 / 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{
                        .Binary = .{
                            .left = &e(.{ .Integer = 3 }),
                            .right = &e(.{ .Integer = 4 }),
                            .operator = .Asterisk,
                        },
                    }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in first pos",
            .input = "(3 * 4) / 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{
                        .Binary = .{
                            .left = &e(.{ .Integer = 3 }),
                            .right = &e(.{ .Integer = 4 }),
                            .operator = .Asterisk,
                        },
                    }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in second pos",
            .input = "3 * (4 / 3)",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{
                        .Binary = .{
                            .left = &e(.{ .Integer = 4 }),
                            .right = &e(.{ .Integer = 3 }),
                            .operator = .Slash,
                        },
                    }),
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float multiplication",
            .input = "5.3 * 66.5",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Float = 5.3 }),
                    .right = &e(.{ .Float = 66.5 }),
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float mod",
            .input = "1.1 % 5.3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Float = 1.1 }),
                    .right = &e(.{ .Float = 5.3 }),
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should parse integer division",
            .input = "6 / 2",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 6 }),
                    .right = &e(.{ .Integer = 2 }),
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse float division",
            .input = "6.55 / 2.413",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Float = 6.55 }),
                    .right = &e(.{ .Float = 2.413 }),
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse integer less than",
            .input = "1 < 5",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 1 }),
                    .right = &e(.{ .Integer = 5 }),
                    .operator = .Lt,
                },
            },
        },
        .{
            .description = "should parse integer less than or equal",
            .input = "1 <= 5",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 1 }),
                    .right = &e(.{ .Integer = 5 }),
                    .operator = .LtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer greater than",
            .input = "1 > 5",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 1 }),
                    .right = &e(.{ .Integer = 5 }),
                    .operator = .Gt,
                },
            },
        },
        .{
            .description = "should parse integer greater than or equal",
            .input = "1 >= 5",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 1 }),
                    .right = &e(.{ .Integer = 5 }),
                    .operator = .GtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer equals",
            .input = "3 == 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse integer does not equal",
            .input = "3 != 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse integer bitwise or",
            .input = "3 | 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .Pipe,
                },
            },
        },
        .{
            .description = "should parse integer bitwise and",
            .input = "3 & 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .Ampersand,
                },
            },
        },
        .{
            .description = "should parse integer bitwise xor",
            .input = "3 ^ 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .Caret,
                },
            },
        },
        .{
            .description = "should parse integer shift left",
            .input = "3 << 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .LeftShift,
                },
            },
        },
        .{
            .description = "should parse integer shift right",
            .input = "3 >> 3",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{ .Integer = 3 }),
                    .operator = .RightShift,
                },
            },
        },
        .{
            .description = "should parse boolean equals",
            .input = "true == false",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Boolean = true }),
                    .right = &e(.{ .Boolean = false }),
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse boolean does not equal",
            .input = "true != false",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Boolean = true }),
                    .right = &e(.{ .Boolean = false }),
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse boolean logical or",
            .input = "true or false",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Boolean = true }),
                    .right = &e(.{ .Boolean = false }),
                    .operator = .Or,
                },
            },
        },
        .{
            .description = "should parse boolean logical and",
            .input = "true and false",
            .expected_expression = .{
                .Binary = .{
                    .left = &e(.{ .Boolean = true }),
                    .right = &e(.{ .Boolean = false }),
                    .operator = .And,
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse binary expression",
        &test_cases,
        runExpressionTest,
    );
}

test "list literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty list",
            .input =
            \\[]
            ,
            .expected_expression = .{ .List = .empty },
        },
        .{
            .description = "empty list with new line",
            .input =
            \\[
            \\
            \\]
            ,
            .expected_expression = .{ .List = .empty },
        },
        .{
            .description = "list one line",
            .input =
            \\[1, 2]
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{ .Integer = 1 }),
                    &e(.{ .Integer = 2 }),
                }),
            },
        },
        .{
            .description = "list one line trailing comma",
            .input =
            \\[1, 2,]
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{ .Integer = 1 }),
                    &e(.{ .Integer = 2 }),
                }),
            },
        },
        .{
            .description = "list multiple lines",
            .input =
            \\[
            \\    1,
            \\    2
            \\]
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{ .Integer = 1 }),
                    &e(.{ .Integer = 2 }),
                }),
            },
        },
        .{
            .description = "list multiple lines trailing comma",
            .input =
            \\[
            \\    1,
            \\    2,
            \\]
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{ .Integer = 1 }),
                    &e(.{ .Integer = 2 }),
                }),
            },
        },
        .{
            .description = "list multiple lines whitespace",
            .input =
            \\[
            \\
            \\    1,
            \\
            \\    2
            \\
            \\]
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{ .Integer = 1 }),
                    &e(.{ .Integer = 2 }),
                }),
            },
        },
        .{
            .description = "list multiple lines trailing comma whitespace",
            .input =
            \\[
            \\
            \\    1,
            \\
            \\    2,
            \\
            \\]
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{ .Integer = 1 }),
                    &e(.{ .Integer = 2 }),
                }),
            },
        },
        .{
            .description = "eof",
            .input =
            \\[ 1, 2,
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma",
            .input =
            \\[ 1 2 ]
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma multiple lines",
            .input =
            \\[
            \\    1
            \\    2
            \\]
            ,
            .expect_error = true,
        },
        .{
            .description = "list of lists",
            .input =
            \\[
            \\    [1, 2],
            \\    [3, 4],
            \\]
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{
                        .List = try test_utils.list(*const ast.Expression, a, &.{
                            &e(.{ .Integer = 1 }),
                            &e(.{ .Integer = 2 }),
                        }),
                    }),
                    &e(.{
                        .List = try test_utils.list(*const ast.Expression, a, &.{
                            &e(.{ .Integer = 3 }),
                            &e(.{ .Integer = 4 }),
                        }),
                    }),
                }),
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse list literal",
        &test_cases,
        runExpressionTest,
    );
}

test "function call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "no args",
            .input =
            \\test()
            ,
            .expected_expression = .{
                .Call = .{
                    .function = &e(.{ .Identifier = "test" }),
                    .args = .empty,
                },
            },
        },
        .{
            .description = "with args one line",
            .input =
            \\test(1, 2)
            ,
            .expected_expression = .{
                .Call = .{
                    .function = &e(.{ .Identifier = "test" }),
                    .args = try test_utils.list(*const ast.Expression, a, &.{
                        &e(.{ .Integer = 1 }),
                        &e(.{ .Integer = 2 }),
                    }),
                },
            },
        },
        .{
            .description = "with args one line trailing comma",
            .input =
            \\test(1, 2,)
            ,
            .expected_expression = .{
                .Call = .{
                    .function = &e(.{ .Identifier = "test" }),
                    .args = try test_utils.list(*const ast.Expression, a, &.{
                        &e(.{ .Integer = 1 }),
                        &e(.{ .Integer = 2 }),
                    }),
                },
            },
        },
        .{
            .description = "with args multiple lines",
            .input =
            \\test(
            \\    1,
            \\    2
            \\)
            ,
            .expected_expression = .{
                .Call = .{
                    .function = &e(.{ .Identifier = "test" }),
                    .args = try test_utils.list(*const ast.Expression, a, &.{
                        &e(.{ .Integer = 1 }),
                        &e(.{ .Integer = 2 }),
                    }),
                },
            },
        },
        .{
            .description = "with args multiple lines trailing comma",
            .input =
            \\test(
            \\    1,
            \\    2,
            \\)
            ,
            .expected_expression = .{
                .Call = .{
                    .function = &e(.{ .Identifier = "test" }),
                    .args = try test_utils.list(*const ast.Expression, a, &.{
                        &e(.{ .Integer = 1 }),
                        &e(.{ .Integer = 2 }),
                    }),
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse function call",
        &test_cases,
        runExpressionTest,
    );
}

test "function literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty",
            .input =
            \\() -> ()
            ,
            .expected_expression = .{
                .Function = .{ .params = .empty, .body = &e(.{ .Unit = {} }) },
            },
        },

        .{
            .description = "with expression",
            .input =
            \\(a, b) -> a + b
            ,
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, a, &.{
                        .{ .Positional = s("a") },
                        .{ .Positional = s("b") },
                    }),
                    .body = &e(.{
                        .Binary = .{
                            .left = &e(.{ .Identifier = "a" }),
                            .operator = .Plus,
                            .right = &e(.{ .Identifier = "b" }),
                        },
                    }),
                },
            },
        },
        .{
            .description = "with grouping",
            .input =
            \\(a, b) -> (
            \\    c = a + b
            \\    a + c
            \\)
            ,
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, a, &.{
                        .{ .Positional = s("a") },
                        .{ .Positional = s("b") },
                    }),
                    .body = &e(.{
                        .Grouping = try test_utils.list(*const ast.Expression, a, &.{
                            &e(.{
                                .Declaration = .{
                                    .left = s("c"),
                                    .right = &e(.{
                                        .Binary = .{
                                            .left = &e(.{ .Identifier = "a" }),
                                            .operator = .Plus,
                                            .right = &e(.{ .Identifier = "b" }),
                                        },
                                    }),
                                },
                            }),
                            &e(.{
                                .Binary = .{
                                    .left = &e(.{ .Identifier = "a" }),
                                    .operator = .Plus,
                                    .right = &e(.{ .Identifier = "c" }),
                                },
                            }),
                        }),
                    }),
                },
            },
        },
        .{
            .description = "with default args",
            .input =
            \\(a = 11, b = 12) -> a + b
            ,
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, a, &.{
                        .{ .Default = .{ .name = s("a"), .value = &e(.{ .Integer = 11 }) } },
                        .{ .Default = .{ .name = s("b"), .value = &e(.{ .Integer = 12 }) } },
                    }),
                    .body = &e(.{
                        .Binary = .{
                            .left = &e(.{ .Identifier = "a" }),
                            .operator = .Plus,
                            .right = &e(.{ .Identifier = "b" }),
                        },
                    }),
                },
            },
        },
        .{
            .description = "declare function",
            .input =
            \\foo = () -> ()
            ,
            .expected_expression = .{
                .Declaration = .{
                    .left = s("foo"),
                    .right = &e(
                        .{
                            .Function = .{
                                .params = .empty,
                                .body = &e(.{ .Unit = {} }),
                                .name = s("foo"),
                            },
                        },
                    ),
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse function literal",
        &test_cases,
        runExpressionTest,
    );
}

test "match expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "one line",
            .input =
            \\match foo with | true -> 42
            ,
            .expected_expression = .{
                .Match = .{
                    .target = &e(.{ .Identifier = "foo" }),
                    .arms = try test_utils.list(ast.MatchArm, a, &.{
                        .{
                            .pattern = &e(.{ .Boolean = true }),
                            .expression = &e(.{ .Integer = 42 }),
                        },
                    }),
                },
            },
        },
        .{
            .description = "multiple lines",
            .input =
            \\match something with 
            \\| "foo" -> 42
            \\| "bar" -> 43
            \\| "baz" -> 44
            ,
            .expected_expression = .{
                .Match = .{
                    .target = &e(.{ .Identifier = "something" }),
                    .arms = try test_utils.list(ast.MatchArm, a, &.{
                        .{
                            .pattern = &e(.{ .String = "foo" }),
                            .expression = &e(.{ .Integer = 42 }),
                        },
                        .{
                            .pattern = &e(.{ .String = "bar" }),
                            .expression = &e(.{ .Integer = 43 }),
                        },
                        .{
                            .pattern = &e(.{ .String = "baz" }),
                            .expression = &e(.{ .Integer = 44 }),
                        },
                    }),
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse match expression",
        &test_cases,
        runExpressionTest,
    );
}

test "if expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "one line",
            .input =
            \\if foo then bar
            ,
            .expected_expression = .{
                .If = .{
                    .arms = try test_utils.list(ast.IfArm, a, &.{
                        .{
                            .condition = &e(.{ .Identifier = "foo" }),
                            .then = &e(.{ .Identifier = "bar" }),
                        },
                    }),
                    .alt = null,
                },
            },
        },
        .{
            .description = "with else",
            .input =
            \\if foo then
            \\    bar
            \\else
            \\    baz
            ,
            .expected_expression = .{
                .If = .{
                    .arms = try test_utils.list(ast.IfArm, a, &.{
                        .{
                            .condition = &e(.{ .Identifier = "foo" }),
                            .then = &e(.{ .Identifier = "bar" }),
                        },
                    }),
                    .alt = &e(.{ .Identifier = "baz" }),
                },
            },
        },
        .{
            .description = "with else if",
            .input =
            \\if foo then 42
            \\elif bar then 43
            \\elif baz then 44
            \\else 45
            ,
            .expected_expression = .{
                .If = .{
                    .arms = try test_utils.list(ast.IfArm, a, &.{
                        .{
                            .condition = &e(.{ .Identifier = "foo" }),
                            .then = &e(.{ .Integer = 42 }),
                        },
                        .{
                            .condition = &e(.{ .Identifier = "bar" }),
                            .then = &e(.{ .Integer = 43 }),
                        },
                        .{
                            .condition = &e(.{ .Identifier = "baz" }),
                            .then = &e(.{ .Integer = 44 }),
                        },
                    }),
                    .alt = &e(.{ .Integer = 45 }),
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse if expression",
        &test_cases,
        runExpressionTest,
    );
}

test "grouping" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "with encasing new lines",
            .input =
            \\(
            \\    a = 1
            \\    b = 1
            \\    a + b
            \\)
            ,
            .expected_expression = .{
                .Grouping = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{
                        .Declaration = .{
                            .left = s("a"),
                            .right = &e(.{ .Integer = 1 }),
                        },
                    }),
                    &e(.{
                        .Declaration = .{
                            .left = s("b"),
                            .right = &e(.{ .Integer = 1 }),
                        },
                    }),
                    &e(.{
                        .Binary = .{
                            .left = &e(.{ .Identifier = "a" }),
                            .right = &e(.{ .Identifier = "b" }),
                            .operator = .Plus,
                        },
                    }),
                }),
            },
        },
        .{
            .description = "without encasing new lines",
            .input =
            \\(a = 1
            \\ b = 1
            \\ a + b)
            ,
            .expected_expression = .{
                .Grouping = try test_utils.list(*const ast.Expression, a, &.{
                    &e(.{
                        .Declaration = .{
                            .left = s("a"),
                            .right = &e(.{ .Integer = 1 }),
                        },
                    }),
                    &e(.{
                        .Declaration = .{
                            .left = s("b"),
                            .right = &e(.{ .Integer = 1 }),
                        },
                    }),
                    &e(.{
                        .Binary = .{
                            .left = &e(.{ .Identifier = "a" }),
                            .right = &e(.{ .Identifier = "b" }),
                            .operator = .Plus,
                        },
                    }),
                }),
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse grouping",
        &test_cases,
        runExpressionTest,
    );
}

// test "parse program" {
//     const ProgramTestCase = struct {
//         description: []const u8,
//         input: []const u8,
//         expected: ast.Program,
//     };
//
//     const run = struct {
//         fn runTest(arena: std.mem.Allocator, test_case: ProgramTestCase) anyerror!void {
//             const ctx = try arena.create(vm.ScriptContext);
//             ctx.* = .{
//                 .file_name = "scanner_test",
//                 .source = test_case.input,
//                 .lines = .empty,
//             };
//
//             const program = try parser.createAst(arena, ctx);
//             try std.testing.expectEqual(test_case.expected.items.len, program.items.len);
//             for (0..program.items.len) |i| {
//                 try expectStatement(test_case.expected.items[i], program.items[i]);
//             }
//         }
//     }.runTest;
//
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     const a = arena.allocator();
//
//     const test_cases = [_]ProgramTestCase{
//         .{
//             .description = "fibonacci",
//             .input =
//             \\let fib = fn(n) {
//             \\    let a = 0
//             \\    let b = 1
//             \\    for (0..n) |_| {
//             \\        let tmp = b
//             \\        b = a + b
//             \\        a = tmp
//             \\    }
//             \\    return a
//             \\}
//             \\
//             \\print(fib(30))
//             ,
//             .expected = try test_utils.list(ast.Statement, a, &.{
//                 .{
//                     .VarDeclaration = .{
//                         .name = s("fib"),
//                         .expression = &e(.{
//                             .Function = .{
//                                 .name = "fib",
//                                 .params = try test_utils.list(ast.FunctionParam, a, &.{
//                                     .{ .Positional = s("n") },
//                                 }),
//                                 .body = &e(.{
//                                     .Block = try test_utils.list(ast.Statement, a, &.{
//                                         .{
//                                             .VarDeclaration = .{
//                                                 .name = s("a"),
//                                                 .expression = &e(.{ .Integer = 0 }),
//                                             },
//                                         },
//                                         .{
//                                             .VarDeclaration = .{
//                                                 .name = s("b"),
//                                                 .expression = &e(.{ .Integer = 1 }),
//                                             },
//                                         },
//                                         .{
//                                             .For = .{
//                                                 .expression = &e(.{
//                                                     .Range = .{
//                                                         .start = &e(.{ .Integer = 0 }),
//                                                         .end = &e(.{ .Identifier = "n" }),
//                                                     },
//                                                 }),
//                                                 .capture = s("_"),
//                                                 .index = null,
//                                                 .body = &.{
//                                                     .Expression = &e(
//                                                         .{
//                                                             .Block = try test_utils.list(
//                                                                 ast.Statement,
//                                                                 a,
//                                                                 &.{
//                                                                     .{
//                                                                         .VarDeclaration = .{
//                                                                             .name = s("tmp"),
//                                                                             .expression = &e(.{ .Identifier = "b" }),
//                                                                         },
//                                                                     },
//                                                                     .{
//                                                                         .Assignment = .{
//                                                                             .target = .{ .Identifier = s("b") },
//                                                                             .expression = &e(.{
//                                                                                 .Infix = .{
//                                                                                     .left = &e(.{ .Identifier = "a" }),
//                                                                                     .right = &e(.{ .Identifier = "b" }),
//                                                                                     .operator = .Plus,
//                                                                                 },
//                                                                             }),
//                                                                         },
//                                                                     },
//                                                                     .{
//                                                                         .Assignment = .{
//                                                                             .target = .{ .Identifier = s("a") },
//                                                                             .expression = &e(.{ .Identifier = "tmp" }),
//                                                                         },
//                                                                     },
//                                                                 },
//                                                             ),
//                                                         },
//                                                     ),
//                                                 },
//                                             },
//                                         },
//                                         .{
//                                             .Return = &e(.{ .Identifier = "a" }),
//                                         },
//                                     }),
//                                 }),
//                             },
//                         }),
//                     },
//                 },
//                 .{
//                     .Expression = &e(.{
//                         .Call = .{
//                             .function = &e(.{ .Identifier = "print" }),
//                             .args = try test_utils.list(ast.FunctionArg, a, &.{
//                                 .{
//                                     .Positional = &e(.{
//                                         .Call = .{
//                                             .function = &e(.{ .Identifier = "fib" }),
//                                             .args = try test_utils.list(ast.FunctionArg, a, &.{
//                                                 .{ .Positional = &e(.{ .Integer = 30 }) },
//                                             }),
//                                         },
//                                     }),
//                                 },
//                             }),
//                         },
//                     }),
//                 },
//             }),
//         },
//     };
//
//     try test_utils.runTestsWithArena(
//         ProgramTestCase,
//         "parse program",
//         &test_cases,
//         run,
//     );
// }
