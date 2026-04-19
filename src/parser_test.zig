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

fn expectStatement(expected: ast.Statement, actual: ast.Statement) anyerror!void {
    try expectTag(expected, actual);

    switch (expected) {
        .VarDeclaration => |stmt| {
            try std.testing.expectEqualStrings(stmt.name.data, actual.VarDeclaration.name.data);
            try expectExpression(stmt.expression.*, actual.VarDeclaration.expression.*);
        },
        .Block => |stmt| {
            try std.testing.expectEqual(stmt.items.len, actual.Block.items.len);
            for (0..stmt.items.len) |i| {
                try expectStatement(stmt.items[i], actual.Block.items[i]);
            }
        },
        .Assignment => |stmt| {
            try expectTag(stmt.target, actual.Assignment.target);
            if (stmt.target == .Identifier) {
                try std.testing.expectEqualStrings(stmt.target.Identifier.data, actual.Assignment.target.Identifier.data);
            } else {
                try expectExpression(stmt.target.Index.left.*, actual.Assignment.target.Index.left.*);
                try expectExpression(stmt.target.Index.index.*, actual.Assignment.target.Index.index.*);
            }
            try expectExpression(stmt.expression.*, actual.Assignment.expression.*);
        },
        .For => |stmt| {
            try expectExpression(stmt.expression.*, actual.For.expression.*);
            try std.testing.expectEqualStrings(stmt.capture.data, actual.For.capture.data);
            if (stmt.index) |index| {
                try std.testing.expectEqualStrings(index.data, actual.For.index.?.data);
            } else {
                try std.testing.expect(actual.For.index == null);
            }
            try std.testing.expectEqual(stmt.body.items.len, actual.For.body.items.len);
            for (0..stmt.body.items.len) |i| {
                try expectStatement(stmt.body.items[i], actual.For.body.items[i]);
            }
        },
        .While => |stmt| {
            try expectExpression(stmt.expression.*, actual.While.expression.*);
            try std.testing.expectEqual(stmt.body.items.len, actual.While.body.items.len);
            for (0..stmt.body.items.len) |i| {
                try expectStatement(stmt.body.items[i], actual.While.body.items[i]);
            }
        },
        .Return => |stmt| {
            try expectExpression(stmt.*, actual.Return.*);
        },
        .Expression => |stmt| {
            try expectExpression(stmt.*, actual.Expression.*);
        },
    }
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
        .Identifier => |ident| try std.testing.expectEqualStrings(ident, actual.Identifier),
        .String => |string| try std.testing.expectEqualStrings(string, actual.String),
        .Integer => |n| try std.testing.expectEqual(n, actual.Integer),
        .Float => |f| try std.testing.expectEqual(f, actual.Float),
        .Boolean => |b| try std.testing.expectEqual(b, actual.Boolean),
        .Null => {},
        .Prefix => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Prefix.operator);
            try expectExpression(expr.expression.*, actual.Prefix.expression.*);
        },
        .Infix => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Infix.operator);
            try expectExpression(expr.left.*, actual.Infix.left.*);
            try expectExpression(expr.right.*, actual.Infix.right.*);
        },
        .Function => |expr| {
            if (expr.name) |name| {
                try std.testing.expectEqualStrings(name, actual.Function.name.?);
            } else {
                try std.testing.expect(actual.Function.name == null);
            }
            try std.testing.expectEqual(expr.params.items.len, actual.Function.params.items.len);
            for (0..expr.params.items.len) |i| {
                const first = expr.params.items[i];
                const second = actual.Function.params.items[i];
                try expectTag(first, second);
                if (first == .Positional) {
                    try std.testing.expectEqualStrings(first.Positional.data, second.Positional.data);
                } else {
                    try std.testing.expectEqualStrings(first.Default.name.data, second.Default.name.data);
                    try expectExpression(first.Default.value.*, second.Default.value.*);
                }
            }
            try expectTag(expr.body, actual.Function.body);
            if (expr.body == .Expression) {
                try expectExpression(expr.body.Expression.*, actual.Function.body.Expression.*);
            } else {
                try std.testing.expectEqual(expr.body.Block.items.len, actual.Function.body.Block.items.len);
                for (0..expr.body.Block.items.len) |i| {
                    try expectStatement(expr.body.Block.items[i], actual.Function.body.Block.items[i]);
                }
            }
        },
        .Range => |expr| {
            try expectExpression(expr.start.*, actual.Range.start.*);
            try expectExpression(expr.end.*, actual.Range.end.*);
        },
        .Call => |expr| {
            try expectExpression(expr.function.*, actual.Call.function.*);
            try std.testing.expectEqual(expr.args.items.len, actual.Call.args.items.len);
            for (0..expr.args.items.len) |i| {
                const first = expr.args.items[i];
                const second = actual.Call.args.items[i];
                try expectTag(first, second);
                if (first == .Positional) {
                    try expectExpression(first.Positional.*, second.Positional.*);
                } else {
                    try std.testing.expectEqualStrings(first.Named.name.data, second.Named.name.data);
                    try expectExpression(first.Named.value.*, second.Named.value.*);
                }
            }
        },
        .List => |expr| {
            try std.testing.expectEqual(expr.items.len, actual.List.items.len);
            for (expr.items, 0..) |item, i| {
                try expectExpression(item.*, actual.List.items[i].*);
            }
        },
        .Table => |expr| {
            try std.testing.expectEqual(expr.items.len, actual.Table.items.len);
            for (expr.items, 0..) |item, i| {
                try expectExpression(item.key.*, actual.Table.items[i].key.*);
                try expectExpression(item.value.*, actual.Table.items[i].value.*);
            }
        },
        .Index => |expr| {
            try expectExpression(expr.left.*, actual.Index.left.*);
            try expectExpression(expr.index.*, actual.Index.index.*);
        },
        .Match => |expr| {
            if (expr.target) |target| {
                try expectExpression(target.*, actual.Match.target.?.*);
            } else {
                try std.testing.expect(actual.Match.target == null);
            }
            try expectTag(expr.body, actual.Match.body);
            if (expr.body == .Single) {
                try expectExpression(expr.body.Single.pattern.*, actual.Match.body.Single.pattern.*);
                try expectStatement(expr.body.Single.body, actual.Match.body.Single.body);
            } else {
                try std.testing.expectEqual(expr.body.Multiple.items.len, actual.Match.body.Multiple.items.len);
                for (0..expr.body.Multiple.items.len) |i| {
                    const first = expr.body.Multiple.items[i];
                    const second = actual.Match.body.Multiple.items[i];
                    try expectExpression(first.pattern.*, second.pattern.*);
                    try expectStatement(first.body, second.body);
                }
            }
        },
    }
}

const StatementTestCase = struct {
    description: []const u8,
    input: []const u8,
    expected_statement: ?ast.Statement = null,
    expect_error: bool = false,
};

fn runStatementTest(arena: std.mem.Allocator, test_case: StatementTestCase) anyerror!void {
    const ctx = try arena.create(vm.ScriptContext);
    ctx.* = .{
        .file_name = "scanner_test",
        .source = test_case.input,
        .lines = .{},
    };

    const result = parser.createAst(arena, ctx);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const program = try result;
        try std.testing.expectEqual(program.items.len, 1);
        try expectStatement(test_case.expected_statement.?, program.items[0]);
    }
}

test "statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]StatementTestCase{
        .{
            .description = "var declaration",
            .input =
            \\let foo = 5
            ,
            .expected_statement = .{
                .VarDeclaration = .{
                    .name = s("foo"),
                    .expression = &e(.{ .Integer = 5 }),
                },
            },
        },
        .{
            .description = "var declaration empty",
            .input =
            \\let foo
            ,
            .expected_statement = .{
                .VarDeclaration = .{
                    .name = s("foo"),
                    .expression = &e(.Null),
                },
            },
        },
        .{
            .description = "assignment to identifier",
            .input =
            \\foo = 5
            ,
            .expected_statement = .{
                .Assignment = .{
                    .target = .{ .Identifier = s("foo") },
                    .expression = &e(.{ .Integer = 5 }),
                },
            },
        },
        .{
            .description = "assignment to index",
            .input =
            \\foo[0] = 5
            ,
            .expected_statement = .{
                .Assignment = .{
                    .target = .{
                        .Index = .{
                            .left = &e(.{ .Identifier = "foo" }),
                            .index = &e(.{ .Integer = 0 }),
                        },
                    },
                    .expression = &e(.{ .Integer = 5 }),
                },
            },
        },
        .{
            .description = "return",
            .input =
            \\return 5
            ,
            .expected_statement = .{ .Return = &e(.{ .Integer = 5 }) },
        },
        .{
            .description = "return empty",
            .input =
            \\return
            ,
            .expected_statement = .{ .Return = &e(.Null) },
        },
        .{
            .description = "expression",
            .input =
            \\5 + 5
            ,
            .expected_statement = .{
                .Expression = &e(.{
                    .Infix = .{
                        .left = &e(.{ .Integer = 5 }),
                        .right = &e(.{ .Integer = 5 }),
                        .operator = .Plus,
                    },
                }),
            },
        },
        .{
            .description = "block",
            .input =
            \\{
            \\    let foo = 5
            \\    foo + 2
            \\}
            ,
            .expected_statement = .{
                .Block = try test_utils.list(ast.Statement, a, &.{
                    .{
                        .VarDeclaration = .{
                            .name = s("foo"),
                            .expression = &e(.{ .Integer = 5 }),
                        },
                    },
                    .{
                        .Expression = &e(.{
                            .Infix = .{
                                .left = &e(.{ .Identifier = "foo" }),
                                .right = &e(.{ .Integer = 2 }),
                                .operator = .Plus,
                            },
                        }),
                    },
                }),
            },
        },
        .{
            .description = "block single line",
            .input =
            \\{ 5 + 5 }
            ,
            .expected_statement = .{
                .Block = try test_utils.list(ast.Statement, a, &.{
                    .{
                        .Expression = &e(.{
                            .Infix = .{
                                .left = &e(.{ .Integer = 5 }),
                                .right = &e(.{ .Integer = 5 }),
                                .operator = .Plus,
                            },
                        }),
                    },
                }),
            },
        },
        .{
            .description = "block empty",
            .input =
            \\{}
            ,
            .expected_statement = .{ .Block = .{} },
        },
        .{
            .description = "block empty multiple lines",
            .input =
            \\{
            \\
            \\}
            ,
            .expected_statement = .{ .Block = .{} },
        },
        .{
            .description = "while",
            .input =
            \\while(foo < 5) {
            \\    foo = foo + 1
            \\}
            ,
            .expected_statement = .{
                .While = .{
                    .expression = &e(.{
                        .Infix = .{
                            .left = &e(.{ .Identifier = "foo" }),
                            .right = &e(.{ .Integer = 5 }),
                            .operator = .Lt,
                        },
                    }),
                    .body = try test_utils.list(ast.Statement, a, &.{
                        .{
                            .Assignment = .{
                                .target = .{ .Identifier = s("foo") },
                                .expression = &e(.{
                                    .Infix = .{
                                        .left = &e(.{ .Identifier = "foo" }),
                                        .right = &e(.{ .Integer = 1 }),
                                        .operator = .Plus,
                                    },
                                }),
                            },
                        },
                    }),
                },
            },
        },
        .{
            .description = "for",
            .input =
            \\for(0..5) |i| {
            \\    print(i)
            \\}
            ,
            .expected_statement = .{
                .For = .{
                    .expression = &e(.{
                        .Range = .{
                            .start = &e(.{ .Integer = 0 }),
                            .end = &e(.{ .Integer = 5 }),
                        },
                    }),
                    .capture = s("i"),
                    .index = null,
                    .body = try test_utils.list(ast.Statement, a, &.{
                        .{
                            .Expression = &e(.{
                                .Call = .{
                                    .function = &e(.{ .Identifier = "print" }),
                                    .args = try test_utils.list(ast.FunctionArg, a, &.{
                                        .{ .Positional = &e(.{ .Identifier = "i" }) },
                                    }),
                                },
                            }),
                        },
                    }),
                },
            },
        },
        .{
            .description = "for with index",
            .input =
            \\for(foo) |item, i| {
            \\    print(i)
            \\}
            ,
            .expected_statement = .{
                .For = .{
                    .expression = &e(.{ .Identifier = "foo" }),
                    .capture = s("item"),
                    .index = s("i"),
                    .body = try test_utils.list(ast.Statement, a, &.{
                        .{
                            .Expression = &e(.{
                                .Call = .{
                                    .function = &e(.{ .Identifier = "print" }),
                                    .args = try test_utils.list(ast.FunctionArg, a, &.{
                                        .{ .Positional = &e(.{ .Identifier = "i" }) },
                                    }),
                                },
                            }),
                        },
                    }),
                },
            },
        },
        .{
            .description = "bad capture",
            .input =
            \\for(foo) |item i| {
            \\    print(i)
            \\}
            ,
            .expect_error = true,
        },
    };

    try test_utils.runTestsWithArena(
        StatementTestCase,
        "parse statements",
        &test_cases,
        runStatementTest,
    );
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
        .lines = .{},
    };

    const result = parser.createAst(arena, ctx);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const program = try result;
        try std.testing.expectEqual(program.items.len, 1);
        try std.testing.expect(program.items[0] == .Expression);
        try expectExpressionData(test_case.expected_expression.?, program.items[0].Expression.*.data);
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
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse expressions",
        &test_cases,
        runExpressionTest,
    );
}

test "prefix expressions" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse bang operator with true",
            .input = "!true",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Boolean = true }),
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse bang operator with false",
            .input = "!false",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Boolean = false }),
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse tilde operator with false",
            .input = "~false",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Boolean = false }),
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with integer",
            .input = "+5",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Integer = 5 }),
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with integer",
            .input = "-2",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Integer = 2 }),
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse tilde operator with integer",
            .input = "~5",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Integer = 5 }),
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with float",
            .input = "+5.41",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Float = 5.41 }),
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with float",
            .input = "-2.1234",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &e(.{ .Float = 2.1234 }),
                    .operator = .Minus,
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse prefix expression",
        &test_cases,
        runExpressionTest,
    );
}

test "infix expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse integer addition",
            .input = "1 + 1",
            .expected_expression = .{
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
                    .left = &e(.{
                        .Infix = .{
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
                .Infix = .{
                    .left = &e(.{
                        .Infix = .{
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
                .Infix = .{
                    .left = &e(.{ .Integer = 3 }),
                    .right = &e(.{
                        .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
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
                .Infix = .{
                    .left = &e(.{ .Boolean = true }),
                    .right = &e(.{ .Boolean = false }),
                    .operator = .And,
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse infix expression",
        &test_cases,
        runExpressionTest,
    );
}

test "range expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "two integers",
            .input =
            \\0..10
            ,
            .expected_expression = .{
                .Range = .{
                    .start = &e(.{ .Integer = 0 }),
                    .end = &e(.{ .Integer = 10 }),
                },
            },
        },
        .{
            .description = "two infix expressions",
            .input =
            \\2 + 3..50 - 10
            ,
            .expected_expression = .{
                .Range = .{
                    .start = &e(.{
                        .Infix = .{
                            .operator = .Plus,
                            .left = &e(.{ .Integer = 2 }),
                            .right = &e(.{ .Integer = 3 }),
                        },
                    }),
                    .end = &e(.{
                        .Infix = .{
                            .operator = .Minus,
                            .left = &e(.{ .Integer = 50 }),
                            .right = &e(.{ .Integer = 10 }),
                        },
                    }),
                },
            },
        },
        .{
            .description = "two function calls",
            .input =
            \\start()..end()
            ,
            .expected_expression = .{
                .Range = .{
                    .start = &e(.{
                        .Call = .{
                            .args = .{},
                            .function = &e(.{ .Identifier = "start" }),
                        },
                    }),
                    .end = &e(.{
                        .Call = .{
                            .args = .{},
                            .function = &e(.{ .Identifier = "end" }),
                        },
                    }),
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse range expression",
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
            \\List{}
            ,
            .expected_expression = .{ .List = .{} },
        },
        .{
            .description = "empty list with new line",
            .input =
            \\List{
            \\
            \\}
            ,
            .expected_expression = .{ .List = .{} },
        },
        .{
            .description = "list one line",
            .input =
            \\List{1, 2}
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
            \\List{1, 2,}
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
            \\List{ 
            \\    1,
            \\    2
            \\}
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
            \\List{ 
            \\    1,
            \\    2,
            \\}
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
            \\List{ 
            \\
            \\    1,
            \\
            \\    2
            \\
            \\}
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
            \\List{ 
            \\
            \\    1,
            \\
            \\    2,
            \\
            \\}
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
            \\List{ 1, 2,
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma",
            .input =
            \\List{ 1 2 }
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma multiple lines",
            .input =
            \\List{ 
            \\    1 
            \\    2 
            \\}
            ,
            .expect_error = true,
        },
        .{
            .description = "list of lists",
            .input =
            \\List{ 
            \\    List{1, 2},
            \\    List{3, 4},
            \\}
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

test "table literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty table",
            .input =
            \\Table{}
            ,
            .expected_expression = .{ .Table = .{} },
        },
        .{
            .description = "empty table with new line",
            .input =
            \\Table{
            \\
            \\}
            ,
            .expected_expression = .{ .Table = .{} },
        },
        .{
            .description = "table one line",
            .input =
            \\Table{"a" = 1, "b" = 2}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, a, &.{
                    .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 1 }) },
                    .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 2 }) },
                }),
            },
        },
        .{
            .description = "table one line trailing comma",
            .input =
            \\Table{"a" = 1, "b" = 2,}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, a, &.{
                    .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 1 }) },
                    .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 2 }) },
                }),
            },
        },
        .{
            .description = "table multiple lines",
            .input =
            \\Table{
            \\    "a" = 1, 
            \\    "b" = 2
            \\}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, a, &.{
                    .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 1 }) },
                    .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 2 }) },
                }),
            },
        },
        .{
            .description = "table multiple lines trailing comma",
            .input =
            \\Table{
            \\    "a" = 1, 
            \\    "b" = 2,
            \\}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, a, &.{
                    .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 1 }) },
                    .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 2 }) },
                }),
            },
        },
        .{
            .description = "table multiple lines whitespace",
            .input =
            \\Table{
            \\
            \\    "a" = 1, 
            \\
            \\    "b" = 2
            \\
            \\}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, a, &.{
                    .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 1 }) },
                    .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 2 }) },
                }),
            },
        },
        .{
            .description = "table multiple lines trailing comma whitespace",
            .input =
            \\Table{
            \\
            \\    "a" = 1, 
            \\
            \\    "b" = 2,
            \\
            \\}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, a, &.{
                    .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 1 }) },
                    .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 2 }) },
                }),
            },
        },
        .{
            .description = "eof",
            .input =
            \\Table{"a" = 1, "b" = 2
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma",
            .input =
            \\Table{"a" = 1 "b" = 2}
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma multiple lines",
            .input =
            \\Table{
            \\    "a" = 1 
            \\    "b" = 2
            \\}
            ,
            .expect_error = true,
        },
        .{
            .description = "table of tables",
            .input =
            \\Table{
            \\    "a" = Table{"a" = 1, "b" = 2},
            \\    "b" = Table{"a" = 3, "b" = 4},
            \\}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, a, &.{
                    .{
                        .key = &e(.{ .String = "a" }),
                        .value = &e(.{
                            .Table = try test_utils.list(ast.TablePair, a, &.{
                                .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 1 }) },
                                .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 2 }) },
                            }),
                        }),
                    },
                    .{
                        .key = &e(.{ .String = "b" }),
                        .value = &e(.{
                            .Table = try test_utils.list(ast.TablePair, a, &.{
                                .{ .key = &e(.{ .String = "a" }), .value = &e(.{ .Integer = 3 }) },
                                .{ .key = &e(.{ .String = "b" }), .value = &e(.{ .Integer = 4 }) },
                            }),
                        }),
                    },
                }),
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse table literal",
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
                    .args = .{},
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
                    .args = try test_utils.list(ast.FunctionArg, a, &.{
                        .{ .Positional = &e(.{ .Integer = 1 }) },
                        .{ .Positional = &e(.{ .Integer = 2 }) },
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
                    .args = try test_utils.list(ast.FunctionArg, a, &.{
                        .{ .Positional = &e(.{ .Integer = 1 }) },
                        .{ .Positional = &e(.{ .Integer = 2 }) },
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
                    .args = try test_utils.list(ast.FunctionArg, a, &.{
                        .{ .Positional = &e(.{ .Integer = 1 }) },
                        .{ .Positional = &e(.{ .Integer = 2 }) },
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
                    .args = try test_utils.list(ast.FunctionArg, a, &.{
                        .{ .Positional = &e(.{ .Integer = 1 }) },
                        .{ .Positional = &e(.{ .Integer = 2 }) },
                    }),
                },
            },
        },
        .{
            .description = "with named args",
            .input =
            \\test(
            \\    1, 
            \\    c=2,
            \\    b=3,
            \\)
            ,
            .expected_expression = .{
                .Call = .{
                    .function = &e(.{ .Identifier = "test" }),
                    .args = try test_utils.list(ast.FunctionArg, a, &.{
                        .{ .Positional = &e(.{ .Integer = 1 }) },
                        .{ .Named = .{ .name = s("c"), .value = &e(.{ .Integer = 2 }) } },
                        .{ .Named = .{ .name = s("b"), .value = &e(.{ .Integer = 3 }) } },
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
            \\fn() {
            \\
            \\}
            ,
            .expected_expression = .{
                .Function = .{ .params = .{}, .body = .{ .Block = .{} } },
            },
        },
        .{
            .description = "with block",
            .input =
            \\fn(a, b) {
            \\    return a + b
            \\}
            ,
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, a, &.{
                        .{ .Positional = s("a") },
                        .{ .Positional = s("b") },
                    }),
                    .body = .{
                        .Block = try test_utils.list(ast.Statement, a, &.{
                            .{
                                .Return = &e(.{
                                    .Infix = .{
                                        .left = &e(.{ .Identifier = "a" }),
                                        .operator = .Plus,
                                        .right = &e(.{ .Identifier = "b" }),
                                    },
                                }),
                            },
                        }),
                    },
                },
            },
        },
        .{
            .description = "with expression",
            .input =
            \\fn(a, b) a + b
            ,
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, a, &.{
                        .{ .Positional = s("a") },
                        .{ .Positional = s("b") },
                    }),
                    .body = .{
                        .Expression = &e(.{
                            .Infix = .{
                                .left = &e(.{ .Identifier = "a" }),
                                .operator = .Plus,
                                .right = &e(.{ .Identifier = "b" }),
                            },
                        }),
                    },
                },
            },
        },
        .{
            .description = "with default args",
            .input =
            \\fn(a = 11, b = 12) a + b
            ,
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, a, &.{
                        .{ .Default = .{ .name = s("a"), .value = &e(.{ .Integer = 11 }) } },
                        .{ .Default = .{ .name = s("b"), .value = &e(.{ .Integer = 12 }) } },
                    }),
                    .body = .{
                        .Expression = &e(.{
                            .Infix = .{
                                .left = &e(.{ .Identifier = "a" }),
                                .operator = .Plus,
                                .right = &e(.{ .Identifier = "b" }),
                            },
                        }),
                    },
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

test "index expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "index",
            .input =
            \\a[2]
            ,
            .expected_expression = .{
                .Index = .{
                    .left = &e(.{ .Identifier = "a" }),
                    .index = &e(.{ .Integer = 2 }),
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse index expression",
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
            .description = "single arm",
            .input =
            \\match(foo) true -> return "bar"
            ,
            .expected_expression = .{
                .Match = .{
                    .target = &e(.{ .Identifier = "foo" }),
                    .body = .{
                        .Single = .{
                            .pattern = &e(.{ .Boolean = true }),
                            .body = .{ .Return = &e(.{ .String = "bar" }) },
                        },
                    },
                },
            },
        },
        .{
            .description = "single arm no target",
            .input =
            \\match foo < 10 -> return "bar"
            ,
            .expected_expression = .{
                .Match = .{
                    .target = null,
                    .body = .{
                        .Single = .{
                            .pattern = &e(.{
                                .Infix = .{
                                    .left = &e(.{ .Identifier = "foo" }),
                                    .right = &e(.{ .Integer = 10 }),
                                    .operator = .Lt,
                                },
                            }),
                            .body = .{ .Return = &e(.{ .String = "bar" }) },
                        },
                    },
                },
            },
        },
        .{
            .description = "multiple arms",
            .input =
            \\match(foo) {
            \\     1 ->    return "a"
            \\     2 ->    return "b"
            \\     _ ->    return "c"
            \\}
            ,
            .expected_expression = .{
                .Match = .{
                    .target = &e(.{ .Identifier = "foo" }),
                    .body = .{
                        .Multiple = try test_utils.list(ast.MatchArm, a, &.{
                            .{
                                .pattern = &e(.{ .Integer = 1 }),
                                .body = .{ .Return = &e(.{ .String = "a" }) },
                            },
                            .{
                                .pattern = &e(.{ .Integer = 2 }),
                                .body = .{ .Return = &e(.{ .String = "b" }) },
                            },
                            .{
                                .pattern = &e(.{ .Identifier = "_" }),
                                .body = .{ .Return = &e(.{ .String = "c" }) },
                            },
                        }),
                    },
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

test "parse program" {
    const ProgramTestCase = struct {
        description: []const u8,
        input: []const u8,
        expected: ast.Program,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: ProgramTestCase) anyerror!void {
            const ctx = try arena.create(vm.ScriptContext);
            ctx.* = .{
                .file_name = "scanner_test",
                .source = test_case.input,
                .lines = .{},
            };

            const program = try parser.createAst(arena, ctx);
            try std.testing.expectEqual(test_case.expected.items.len, program.items.len);
            for (0..program.items.len) |i| {
                try expectStatement(test_case.expected.items[i], program.items[i]);
            }
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const test_cases = [_]ProgramTestCase{
        .{
            .description = "fibonacci",
            .input =
            \\let fib = fn(n) {
            \\    let a = 0
            \\    let b = 1
            \\    for (0..n) |_| {
            \\        let tmp = b
            \\        b = a + b
            \\        a = tmp
            \\    }
            \\    return a
            \\}
            \\
            \\print(fib(30))
            ,
            .expected = try test_utils.list(ast.Statement, a, &.{
                .{
                    .VarDeclaration = .{
                        .name = s("fib"),
                        .expression = &e(.{
                            .Function = .{
                                .name = "fib",
                                .params = try test_utils.list(ast.FunctionParam, a, &.{
                                    .{ .Positional = s("n") },
                                }),
                                .body = .{
                                    .Block = try test_utils.list(ast.Statement, a, &.{
                                        .{
                                            .VarDeclaration = .{
                                                .name = s("a"),
                                                .expression = &e(.{ .Integer = 0 }),
                                            },
                                        },
                                        .{
                                            .VarDeclaration = .{
                                                .name = s("b"),
                                                .expression = &e(.{ .Integer = 1 }),
                                            },
                                        },
                                        .{
                                            .For = .{
                                                .expression = &e(.{
                                                    .Range = .{
                                                        .start = &e(.{ .Integer = 0 }),
                                                        .end = &e(.{ .Identifier = "n" }),
                                                    },
                                                }),
                                                .capture = s("_"),
                                                .index = null,
                                                .body = try test_utils.list(ast.Statement, a, &.{
                                                    .{
                                                        .VarDeclaration = .{
                                                            .name = s("tmp"),
                                                            .expression = &e(.{ .Identifier = "b" }),
                                                        },
                                                    },
                                                    .{
                                                        .Assignment = .{
                                                            .target = .{ .Identifier = s("b") },
                                                            .expression = &e(.{
                                                                .Infix = .{
                                                                    .left = &e(.{ .Identifier = "a" }),
                                                                    .right = &e(.{ .Identifier = "b" }),
                                                                    .operator = .Plus,
                                                                },
                                                            }),
                                                        },
                                                    },
                                                    .{
                                                        .Assignment = .{
                                                            .target = .{ .Identifier = s("a") },
                                                            .expression = &e(.{ .Identifier = "tmp" }),
                                                        },
                                                    },
                                                }),
                                            },
                                        },
                                        .{
                                            .Return = &e(.{ .Identifier = "a" }),
                                        },
                                    }),
                                },
                            },
                        }),
                    },
                },
                .{
                    .Expression = &e(.{
                        .Call = .{
                            .function = &e(.{ .Identifier = "print" }),
                            .args = try test_utils.list(ast.FunctionArg, a, &.{
                                .{
                                    .Positional = &e(.{
                                        .Call = .{
                                            .function = &e(.{ .Identifier = "fib" }),
                                            .args = try test_utils.list(ast.FunctionArg, a, &.{
                                                .{ .Positional = &e(.{ .Integer = 30 }) },
                                            }),
                                        },
                                    }),
                                },
                            }),
                        },
                    }),
                },
            }),
        },
    };

    try test_utils.runTestsWithArena(
        ProgramTestCase,
        "parse program",
        &test_cases,
        run,
    );
}
