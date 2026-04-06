const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");
const parser = @import("parser.zig");

const Statement = parser.Statement;
const Expression = parser.Expression;

fn expectStatement(expected: Statement, actual: Statement) anyerror!void {
    try expectTag(expected, actual);

    switch (expected) {
        .VarDeclaration => |stmt| {
            try std.testing.expectEqualStrings(stmt.name, actual.VarDeclaration.name);
            try expectExpression(stmt.expression.*, actual.VarDeclaration.expression.*);
        },
        .Block => |stmt| {
            for (0..stmt.items.len) |i| {
                try expectStatement(stmt.items[i], actual.Block.items[i]);
            }
        },
        .Assignment => |stmt| {
            try expectTag(stmt.target, actual.Assignment.target);
            if (stmt.target == .Identifier) {
                try std.testing.expectEqualStrings(stmt.target.Identifier, actual.Assignment.target.Identifier);
            } else {
                try expectExpression(stmt.target.Index.left.*, actual.Assignment.target.Index.left.*);
                try expectExpression(stmt.target.Index.index.*, actual.Assignment.target.Index.index.*);
            }
            try expectExpression(stmt.expression.*, actual.Assignment.expression.*);
        },
        .For => |stmt| {
            try expectExpression(stmt.expression.*, actual.For.expression.*);
            try std.testing.expectEqualStrings(stmt.capture, actual.For.capture);
            if (stmt.index) |index| {
                try std.testing.expectEqualStrings(index, actual.For.index.?);
            } else {
                try std.testing.expect(actual.For.index == null);
            }
            for (0..stmt.body.items.len) |i| {
                try expectStatement(stmt.body.items[i], actual.For.body.items[i]);
            }
        },
        .While => |stmt| {
            try expectExpression(stmt.expression.*, actual.While.expression.*);
            for (0..stmt.body.items.len) |i| {
                try expectStatement(stmt.body.items[i], actual.While.body.items[i]);
            }
        },
        .Return => |stmt| {
            try expectExpression(stmt.*, actual.Return.*);
        },
        .Assert => |stmt| {
            try expectExpression(stmt.*, actual.Assert.*);
        },
        .Print => |stmt| {
            try expectExpression(stmt.*, actual.Print.*);
        },
        .Expression => |stmt| {
            try expectExpression(stmt.*, actual.Expression.*);
        },
    }
}

fn expectTag(expected: anytype, actual: anytype) !void {
    try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
}

fn expectExpression(expected: Expression, actual: Expression) !void {
    try expectTag(expected, actual);

    switch (expected) {
        .Identifier => |ident| try std.testing.expectEqualStrings(ident, actual.Identifier),
        .String => |string| try std.testing.expectEqualStrings(string, actual.String),
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
            try std.testing.expectEqual(expr.params.items.len, actual.Function.params.items.len);
            for (0..expr.params.items.len) |i| {
                const first = expr.params.items[i];
                const second = actual.Function.params.items[i];
                try expectTag(first, second);
                if (first == .Positional) {
                    try std.testing.expectEqualStrings(first.Positional, second.Positional);
                } else {
                    try std.testing.expectEqualStrings(first.Default.name, second.Default.name);
                    try expectExpression(first.Default.value.*, second.Default.value.*);
                }
            }
            try expectTag(expr.body, actual.Function.body);
            if (expr.body == .Expression) {
                try expectExpression(expr.body.Expression.*, actual.Function.body.Expression.*);
            } else {
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
                    try std.testing.expectEqualStrings(first.Named.name, second.Named.name);
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
        else => try std.testing.expectEqual(expected, actual),
    }
}

const StatementTestCase = struct {
    description: []const u8,
    input: []const u8,
    expected_statement: ?Statement = null,
    expect_error: bool = false,
};

fn runStatementTest(arena: std.mem.Allocator, test_case: StatementTestCase) anyerror!void {
    const result = parser.createAst(arena, test_case.input);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const ast = try result;
        try std.testing.expectEqual(ast.items.len, 1);
        try expectStatement(test_case.expected_statement.?, ast.items[0]);
    }
}

test "statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]StatementTestCase{
        .{
            .description = "var declaration",
            .input =
            \\let foo = 5
            ,
            .expected_statement = Statement{
                .VarDeclaration = .{
                    .name = "foo",
                    .expression = &Expression{ .Integer = 5 },
                },
            },
        },
        .{
            .description = "var declaration empty",
            .input =
            \\let foo
            ,
            .expected_statement = Statement{
                .VarDeclaration = .{
                    .name = "foo",
                    .expression = &Expression{ .Null = {} },
                },
            },
        },
        .{
            .description = "assignment to identifier",
            .input =
            \\foo = 5
            ,
            .expected_statement = Statement{
                .Assignment = .{
                    .target = .{ .Identifier = "foo" },
                    .expression = &Expression{ .Integer = 5 },
                },
            },
        },
        .{
            .description = "assignment to index",
            .input =
            \\foo[0] = 5
            ,
            .expected_statement = Statement{
                .Assignment = .{
                    .target = .{
                        .Index = .{
                            .left = &Expression{ .Identifier = "foo" },
                            .index = &Expression{ .Integer = 0 },
                        },
                    },
                    .expression = &Expression{ .Integer = 5 },
                },
            },
        },
        .{
            .description = "return",
            .input =
            \\return 5
            ,
            .expected_statement = Statement{
                .Return = &Expression{ .Integer = 5 },
            },
        },
        .{
            .description = "return empty",
            .input =
            \\return
            ,
            .expected_statement = Statement{
                .Return = &Expression{ .Null = {} },
            },
        },
        .{
            .description = "print",
            .input =
            \\print(5)
            ,
            .expected_statement = Statement{
                .Print = &Expression{ .Integer = 5 },
            },
        },
        .{
            .description = "assert",
            .input =
            \\assert(5)
            ,
            .expected_statement = Statement{
                .Assert = &Expression{ .Integer = 5 },
            },
        },
        .{
            .description = "expression",
            .input =
            \\5 + 5
            ,
            .expected_statement = Statement{
                .Expression = &Expression{
                    .Infix = .{
                        .left = &Expression{ .Integer = 5 },
                        .right = &Expression{ .Integer = 5 },
                        .operator = .Plus,
                    },
                },
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
            .expected_statement = Statement{
                .Block = try test_utils.list(Statement, arena.allocator(), &.{
                    Statement{
                        .VarDeclaration = .{
                            .name = "foo",
                            .expression = &Expression{ .Integer = 5 },
                        },
                    },
                    Statement{
                        .Expression = &Expression{
                            .Infix = .{
                                .left = &Expression{ .Identifier = "foo" },
                                .right = &Expression{ .Integer = 2 },
                                .operator = .Plus,
                            },
                        },
                    },
                }),
            },
        },
        .{
            .description = "block single line",
            .input =
            \\{ print(1) }
            ,
            .expected_statement = Statement{
                .Block = try test_utils.list(Statement, arena.allocator(), &.{
                    Statement{
                        .Print = &Expression{ .Integer = 1 },
                    },
                }),
            },
        },
        .{
            .description = "block empty",
            .input =
            \\{}
            ,
            .expected_statement = Statement{
                .Block = .{},
            },
        },
        .{
            .description = "block empty multiple lines",
            .input =
            \\{
            \\
            \\}
            ,
            .expected_statement = Statement{
                .Block = .{},
            },
        },
        .{
            .description = "while",
            .input =
            \\while(foo < 5) {
            \\    foo = foo + 1
            \\}
            ,
            .expected_statement = Statement{
                .While = .{
                    .expression = &Expression{
                        .Infix = .{
                            .left = &Expression{ .Identifier = "foo" },
                            .right = &Expression{ .Integer = 5 },
                            .operator = .Lt,
                        },
                    },
                    .body = try test_utils.list(Statement, arena.allocator(), &.{
                        Statement{
                            .Assignment = .{
                                .target = .{ .Identifier = "foo" },
                                .expression = &Expression{
                                    .Infix = .{
                                        .left = &Expression{ .Identifier = "foo" },
                                        .right = &Expression{ .Integer = 1 },
                                        .operator = .Plus,
                                    },
                                },
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
            .expected_statement = Statement{
                .For = .{
                    .expression = &Expression{
                        .Range = .{
                            .start = &Expression{ .Integer = 0 },
                            .end = &Expression{ .Integer = 5 },
                        },
                    },
                    .capture = "i",
                    .index = null,
                    .body = try test_utils.list(Statement, arena.allocator(), &.{
                        Statement{
                            .Print = &Expression{ .Identifier = "i" },
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
            .expected_statement = Statement{
                .For = .{
                    .expression = &Expression{
                        .Identifier = "foo",
                    },
                    .capture = "item",
                    .index = "i",
                    .body = try test_utils.list(Statement, arena.allocator(), &.{
                        Statement{
                            .Print = &Expression{ .Identifier = "i" },
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
    expected_expression: ?Expression = null,
    expect_error: bool = false,
};

fn runExpressionTest(arena: std.mem.Allocator, test_case: ExpressionTestCase) anyerror!void {
    const result = parser.createAst(arena, test_case.input);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const ast = try result;
        try std.testing.expectEqual(ast.items.len, 1);
        try std.testing.expect(ast.items[0] == .Expression);
        try expectExpression(test_case.expected_expression.?, ast.items[0].Expression.*);
    }
}

test "expressions" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse identifier",
            .input =
            \\foo
            ,
            .expected_expression = Expression{ .Identifier = "foo" },
        },
        .{
            .description = "should parse string literal",
            .input =
            \\"foo"
            ,
            .expected_expression = Expression{ .String = "foo" },
        },
        .{
            .description = "should parse integer literal",
            .input =
            \\666
            ,
            .expected_expression = Expression{ .Integer = 666 },
        },
        .{
            .description = "should parse float literal",
            .input =
            \\3.1415
            ,
            .expected_expression = Expression{ .Float = 3.1415 },
        },
        .{
            .description = "should parse true",
            .input =
            \\true
            ,
            .expected_expression = Expression{ .Boolean = true },
        },
        .{
            .description = "should parse false",
            .input =
            \\false
            ,
            .expected_expression = Expression{ .Boolean = false },
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
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Boolean = true,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse bang operator with false",
            .input = "!false",
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Boolean = false,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse tilde operator with false",
            .input = "~false",
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Boolean = false,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with integer",
            .input = "+5",
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Integer = 5,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with integer",
            .input = "-2",
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Integer = 2,
                    },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse tilde operator with integer",
            .input = "~5",
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Integer = 5,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with float",
            .input = "+5.41",
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Float = 5.41,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with float",
            .input = "-2.1234",
            .expected_expression = Expression{
                .Prefix = .{
                    .expression = &Expression{
                        .Float = 2.1234,
                    },
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
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 1 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse float addition",
            .input = "1.1 + 1.35",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Float = 1.1 },
                    .right = &Expression{ .Float = 1.35 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse integer subtraction",
            .input = "40 - 22",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 40 },
                    .right = &Expression{ .Integer = 22 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse float subtraction",
            .input = "40.54 - 22.33",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Float = 40.54 },
                    .right = &Expression{ .Float = 22.33 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse integer multiplication",
            .input = "5 * 66",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 5 },
                    .right = &Expression{ .Integer = 66 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse integer mod",
            .input = "33 % 2",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 33 },
                    .right = &Expression{ .Integer = 2 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should preserve order of operations without parens",
            .input = "3 * 4 / 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{
                        .Infix = .{
                            .left = &Expression{
                                .Integer = 3,
                            },
                            .right = &Expression{
                                .Integer = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in first pos",
            .input = "(3 * 4) / 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{
                        .Infix = .{
                            .left = &Expression{
                                .Integer = 3,
                            },
                            .right = &Expression{
                                .Integer = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in second pos",
            .input = "3 * (4 / 3)",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{
                        .Infix = .{
                            .left = &Expression{
                                .Integer = 4,
                            },
                            .right = &Expression{
                                .Integer = 3,
                            },
                            .operator = .Slash,
                        },
                    },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float multiplication",
            .input = "5.3 * 66.5",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Float = 5.3 },
                    .right = &Expression{ .Float = 66.5 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float mod",
            .input = "1.1 % 5.3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Float = 1.1 },
                    .right = &Expression{ .Float = 5.3 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should parse integer division",
            .input = "6 / 2",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 6 },
                    .right = &Expression{ .Integer = 2 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse float division",
            .input = "6.55 / 2.413",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Float = 6.55 },
                    .right = &Expression{ .Float = 2.413 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse integer less than",
            .input = "1 < 5",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .Lt,
                },
            },
        },
        .{
            .description = "should parse integer less than or equal",
            .input = "1 <= 5",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .LtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer greater than",
            .input = "1 > 5",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .Gt,
                },
            },
        },
        .{
            .description = "should parse integer greater than or equal",
            .input = "1 >= 5",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .GtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer equals",
            .input = "3 == 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse integer does not equal",
            .input = "3 != 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse integer bitwise or",
            .input = "3 | 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Pipe,
                },
            },
        },
        .{
            .description = "should parse integer bitwise and",
            .input = "3 & 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Ampersand,
                },
            },
        },
        .{
            .description = "should parse integer bitwise xor",
            .input = "3 ^ 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Caret,
                },
            },
        },
        .{
            .description = "should parse integer shift left",
            .input = "3 << 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .LeftShift,
                },
            },
        },
        .{
            .description = "should parse integer shift right",
            .input = "3 >> 3",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .RightShift,
                },
            },
        },
        .{
            .description = "should parse boolean equals",
            .input = "true == false",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse boolean does not equal",
            .input = "true != false",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse boolean logical or",
            .input = "true or false",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
                    .operator = .Or,
                },
            },
        },
        .{
            .description = "should parse boolean logical and",
            .input = "true and false",
            .expected_expression = Expression{
                .Infix = .{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
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
            .expected_expression = Expression{
                .Range = .{
                    .start = &Expression{
                        .Integer = 0,
                    },
                    .end = &Expression{
                        .Integer = 10,
                    },
                },
            },
        },
        .{
            .description = "two infix expressions",
            .input =
            \\2 + 3..50 - 10
            ,
            .expected_expression = Expression{
                .Range = .{
                    .start = &Expression{
                        .Infix = .{
                            .operator = .Plus,
                            .left = &Expression{
                                .Integer = 2,
                            },
                            .right = &Expression{
                                .Integer = 3,
                            },
                        },
                    },
                    .end = &Expression{
                        .Infix = .{
                            .operator = .Minus,
                            .left = &Expression{
                                .Integer = 50,
                            },
                            .right = &Expression{
                                .Integer = 10,
                            },
                        },
                    },
                },
            },
        },
        .{
            .description = "two function calls",
            .input =
            \\start()..end()
            ,
            .expected_expression = Expression{
                .Range = .{
                    .start = &Expression{
                        .Call = .{
                            .args = .{},
                            .function = &Expression{
                                .Identifier = "start",
                            },
                        },
                    },
                    .end = &Expression{
                        .Call = .{
                            .args = .{},
                            .function = &Expression{
                                .Identifier = "end",
                            },
                        },
                    },
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

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty list",
            .input =
            \\List{}
            ,
            .expected_expression = Expression{
                .List = .{},
            },
        },
        .{
            .description = "empty list with new line",
            .input =
            \\List{
            \\
            \\}
            ,
            .expected_expression = Expression{
                .List = .{},
            },
        },
        .{
            .description = "list one line",
            .input =
            \\List{1, 2}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list one line trailing comma",
            .input =
            \\List{1, 2,}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
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
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
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
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
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
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
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
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list of lists",
            .input =
            \\List{ 
            \\    1,
            \\    2,
            \\}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
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
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .List = try test_utils.list(
                        *const Expression,
                        arena.allocator(),
                        &.{
                            &Expression{ .Integer = 1 },
                            &Expression{ .Integer = 2 },
                        },
                    ) },
                    &Expression{ .List = try test_utils.list(
                        *const Expression,
                        arena.allocator(),
                        &.{
                            &Expression{ .Integer = 3 },
                            &Expression{ .Integer = 4 },
                        },
                    ) },
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

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty table",
            .input =
            \\Table{}
            ,
            .expected_expression = Expression{
                .Table = .{},
            },
        },
        .{
            .description = "empty table with new line",
            .input =
            \\Table{
            \\
            \\}
            ,
            .expected_expression = Expression{
                .Table = .{},
            },
        },
        .{
            .description = "table one line",
            .input =
            \\Table{"a" = 1, "b" = 2}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(parser.TablePair, arena.allocator(), &.{
                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "table one line trailing comma",
            .input =
            \\Table{"a" = 1, "b" = 2,}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(parser.TablePair, arena.allocator(), &.{
                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Table = try test_utils.list(parser.TablePair, arena.allocator(), &.{
                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Table = try test_utils.list(parser.TablePair, arena.allocator(), &.{
                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Table = try test_utils.list(parser.TablePair, arena.allocator(), &.{
                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Table = try test_utils.list(parser.TablePair, arena.allocator(), &.{
                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Table = try test_utils.list(parser.TablePair, arena.allocator(), &.{
                    .{
                        .key = &Expression{ .String = "a" },
                        .value = &Expression{
                            .Table = try test_utils.list(
                                parser.TablePair,
                                arena.allocator(),
                                &.{
                                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                                },
                            ),
                        },
                    },
                    .{
                        .key = &Expression{ .String = "b" },
                        .value = &Expression{
                            .Table = try test_utils.list(
                                parser.TablePair,
                                arena.allocator(),
                                &.{
                                    .{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 3 } },
                                    .{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 4 } },
                                },
                            ),
                        },
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

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "no args",
            .input =
            \\test()
            ,
            .expected_expression = Expression{
                .Call = .{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = .{},
                },
            },
        },
        .{
            .description = "with args one line",
            .input =
            \\test(1, 2)
            ,
            .expected_expression = Expression{
                .Call = .{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(parser.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
                    }),
                },
            },
        },
        .{
            .description = "with args one line trailing comma",
            .input =
            \\test(1, 2,)
            ,
            .expected_expression = Expression{
                .Call = .{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(parser.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Call = .{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(parser.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Call = .{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(parser.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
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
            .expected_expression = Expression{
                .Call = .{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(parser.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Named = .{ .name = "c", .value = &Expression{ .Integer = 2 } } },
                        .{ .Named = .{ .name = "b", .value = &Expression{ .Integer = 3 } } },
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

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty",
            .input =
            \\fn() {
            \\
            \\}
            ,
            .expected_expression = Expression{
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
            .expected_expression = Expression{
                .Function = .{
                    .params = try test_utils.list(parser.FunctionParam, arena.allocator(), &.{
                        .{ .Positional = "a" },
                        .{ .Positional = "b" },
                    }),
                    .body = .{
                        .Block = try test_utils.list(Statement, arena.allocator(), &.{
                            .{
                                .Return = &Expression{
                                    .Infix = .{
                                        .left = &Expression{ .Identifier = "a" },
                                        .operator = .Plus,
                                        .right = &Expression{ .Identifier = "b" },
                                    },
                                },
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
            .expected_expression = Expression{
                .Function = .{
                    .params = try test_utils.list(parser.FunctionParam, arena.allocator(), &.{
                        .{ .Positional = "a" },
                        .{ .Positional = "b" },
                    }),
                    .body = .{
                        .Expression = &Expression{
                            .Infix = .{
                                .left = &Expression{ .Identifier = "a" },
                                .operator = .Plus,
                                .right = &Expression{ .Identifier = "b" },
                            },
                        },
                    },
                },
            },
        },
        .{
            .description = "with default args",
            .input =
            \\fn(a = 11, b = 12) a + b
            ,
            .expected_expression = Expression{
                .Function = .{
                    .params = try test_utils.list(parser.FunctionParam, arena.allocator(), &.{
                        .{ .Default = .{ .name = "a", .value = &Expression{ .Integer = 11 } } },
                        .{ .Default = .{ .name = "b", .value = &Expression{ .Integer = 12 } } },
                    }),
                    .body = .{
                        .Expression = &Expression{
                            .Infix = .{
                                .left = &Expression{ .Identifier = "a" },
                                .operator = .Plus,
                                .right = &Expression{ .Identifier = "b" },
                            },
                        },
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
                    .left = &.{ .Identifier = "a" },
                    .index = &.{ .Integer = 2 },
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

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "single arm",
            .input =
            \\match(foo) true -> return "bar"
            ,
            .expected_expression = .{
                .Match = .{
                    .target = &.{ .Identifier = "foo" },
                    .body = .{
                        .Single = .{
                            .pattern = &.{ .Boolean = true },
                            .body = .{ .Return = &.{ .String = "bar" } },
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
                            .pattern = &.{
                                .Infix = .{
                                    .left = &.{ .Identifier = "foo" },
                                    .right = &.{ .Integer = 10 },
                                    .operator = .Lt,
                                },
                            },
                            .body = .{ .Return = &.{ .String = "bar" } },
                        },
                    },
                },
            },
        },
        .{
            .description = "multiple arms",
            .input =
            \\match(foo) {
            \\     1 ->    print("a")
            \\     2 ->    print("b")
            \\     _ ->    print("c")
            \\}
            ,
            .expected_expression = .{
                .Match = .{
                    .target = &.{ .Identifier = "foo" },
                    .body = .{
                        .Multiple = try test_utils.list(parser.MatchArm, arena.allocator(), &.{
                            .{
                                .pattern = &.{ .Integer = 1 },
                                .body = .{ .Print = &.{ .String = "a" } },
                            },
                            .{
                                .pattern = &.{ .Integer = 2 },
                                .body = .{ .Print = &.{ .String = "b" } },
                            },
                            .{
                                .pattern = &.{ .Identifier = "_" },
                                .body = .{ .Print = &.{ .String = "c" } },
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
        expected: parser.Program,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: ProgramTestCase) anyerror!void {
            const program = try parser.createAst(arena, test_case.input);
            try std.testing.expectEqual(test_case.expected.items.len, program.items.len);
            for (0..program.items.len) |i| {
                try expectStatement(test_case.expected.items[i], program.items[i]);
            }
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

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
            .expected = try test_utils.list(Statement, arena.allocator(), &.{
                .{
                    .VarDeclaration = .{
                        .name = "fib",
                        .expression = &.{
                            .Function = .{
                                .params = try test_utils.list(parser.FunctionParam, arena.allocator(), &.{
                                    .{ .Positional = "n" },
                                }),
                                .body = .{
                                    .Block = try test_utils.list(Statement, arena.allocator(), &.{
                                        .{
                                            .VarDeclaration = .{
                                                .name = "a",
                                                .expression = &.{ .Integer = 0 },
                                            },
                                        },
                                        .{
                                            .VarDeclaration = .{
                                                .name = "b",
                                                .expression = &.{ .Integer = 1 },
                                            },
                                        },
                                        .{
                                            .For = .{
                                                .expression = &.{
                                                    .Range = .{
                                                        .start = &.{ .Integer = 0 },
                                                        .end = &.{ .Identifier = "n" },
                                                    },
                                                },
                                                .capture = "_",
                                                .index = null,
                                                .body = try test_utils.list(Statement, arena.allocator(), &.{
                                                    .{
                                                        .VarDeclaration = .{
                                                            .name = "tmp",
                                                            .expression = &.{ .Identifier = "b" },
                                                        },
                                                    },
                                                    .{
                                                        .Assignment = .{
                                                            .target = .{ .Identifier = "b" },
                                                            .expression = &.{
                                                                .Infix = .{
                                                                    .left = &.{
                                                                        .Identifier = "a",
                                                                    },
                                                                    .right = &.{
                                                                        .Identifier = "b",
                                                                    },
                                                                    .operator = .Plus,
                                                                },
                                                            },
                                                        },
                                                    },
                                                    .{
                                                        .Assignment = .{
                                                            .target = .{ .Identifier = "a" },
                                                            .expression = &.{
                                                                .Identifier = "tmp",
                                                            },
                                                        },
                                                    },
                                                }),
                                            },
                                        },
                                        .{
                                            .Return = &.{ .Identifier = "a" },
                                        },
                                    }),
                                },
                            },
                        },
                    },
                },
                .{
                    .Print = &.{
                        .Call = .{
                            .function = &.{ .Identifier = "fib" },
                            .args = try test_utils.list(parser.FunctionArg, arena.allocator(), &.{
                                .{
                                    .Positional = &.{ .Integer = 30 },
                                },
                            }),
                        },
                    },
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
