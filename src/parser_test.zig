const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

fn expectStatement(expected: ast.Statement, actual: ast.Statement) anyerror!void {
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

fn expectExpression(expected: ast.Expression, actual: ast.Expression) !void {
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
    expected_statement: ?ast.Statement = null,
    expect_error: bool = false,
};

fn runStatementTest(arena: std.mem.Allocator, test_case: StatementTestCase) anyerror!void {
    const result = parser.createAst(arena, test_case.input);

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

    const test_cases = [_]StatementTestCase{
        .{
            .description = "var declaration",
            .input =
            \\let foo = 5
            ,
            .expected_statement = .{
                .VarDeclaration = .{
                    .name = "foo",
                    .expression = &.{ .Integer = 5 },
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
                    .name = "foo",
                    .expression = &.{ .Null = {} },
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
                    .target = .{ .Identifier = "foo" },
                    .expression = &.{ .Integer = 5 },
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
                            .left = &.{ .Identifier = "foo" },
                            .index = &.{ .Integer = 0 },
                        },
                    },
                    .expression = &.{ .Integer = 5 },
                },
            },
        },
        .{
            .description = "return",
            .input =
            \\return 5
            ,
            .expected_statement = .{
                .Return = &.{ .Integer = 5 },
            },
        },
        .{
            .description = "return empty",
            .input =
            \\return
            ,
            .expected_statement = .{
                .Return = &.{ .Null = {} },
            },
        },
        .{
            .description = "print",
            .input =
            \\print(5)
            ,
            .expected_statement = .{
                .Print = &.{ .Integer = 5 },
            },
        },
        .{
            .description = "assert",
            .input =
            \\assert(5)
            ,
            .expected_statement = .{
                .Assert = &.{ .Integer = 5 },
            },
        },
        .{
            .description = "expression",
            .input =
            \\5 + 5
            ,
            .expected_statement = .{
                .Expression = &.{
                    .Infix = .{
                        .left = &.{ .Integer = 5 },
                        .right = &.{ .Integer = 5 },
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
            .expected_statement = .{
                .Block = try test_utils.list(ast.Statement, arena.allocator(), &.{
                    .{
                        .VarDeclaration = .{
                            .name = "foo",
                            .expression = &.{ .Integer = 5 },
                        },
                    },
                    .{
                        .Expression = &.{
                            .Infix = .{
                                .left = &.{ .Identifier = "foo" },
                                .right = &.{ .Integer = 2 },
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
            .expected_statement = .{
                .Block = try test_utils.list(ast.Statement, arena.allocator(), &.{
                    .{
                        .Print = &.{ .Integer = 1 },
                    },
                }),
            },
        },
        .{
            .description = "block empty",
            .input =
            \\{}
            ,
            .expected_statement = .{
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
            .expected_statement = .{
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
            .expected_statement = .{
                .While = .{
                    .expression = &.{
                        .Infix = .{
                            .left = &.{ .Identifier = "foo" },
                            .right = &.{ .Integer = 5 },
                            .operator = .Lt,
                        },
                    },
                    .body = try test_utils.list(ast.Statement, arena.allocator(), &.{
                        .{
                            .Assignment = .{
                                .target = .{ .Identifier = "foo" },
                                .expression = &.{
                                    .Infix = .{
                                        .left = &.{ .Identifier = "foo" },
                                        .right = &.{ .Integer = 1 },
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
            .expected_statement = .{
                .For = .{
                    .expression = &.{
                        .Range = .{
                            .start = &.{ .Integer = 0 },
                            .end = &.{ .Integer = 5 },
                        },
                    },
                    .capture = "i",
                    .index = null,
                    .body = try test_utils.list(ast.Statement, arena.allocator(), &.{
                        .{
                            .Print = &.{ .Identifier = "i" },
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
                    .expression = &.{
                        .Identifier = "foo",
                    },
                    .capture = "item",
                    .index = "i",
                    .body = try test_utils.list(ast.Statement, arena.allocator(), &.{
                        .{
                            .Print = &.{ .Identifier = "i" },
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
    expected_expression: ?ast.Expression = null,
    expect_error: bool = false,
};

fn runExpressionTest(arena: std.mem.Allocator, test_case: ExpressionTestCase) anyerror!void {
    const result = parser.createAst(arena, test_case.input);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const program = try result;
        try std.testing.expectEqual(program.items.len, 1);
        try std.testing.expect(program.items[0] == .Expression);
        try expectExpression(test_case.expected_expression.?, program.items[0].Expression.*);
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
                    .expression = &.{
                        .Boolean = true,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse bang operator with false",
            .input = "!false",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &.{
                        .Boolean = false,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse tilde operator with false",
            .input = "~false",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &.{
                        .Boolean = false,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with integer",
            .input = "+5",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &.{
                        .Integer = 5,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with integer",
            .input = "-2",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &.{
                        .Integer = 2,
                    },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse tilde operator with integer",
            .input = "~5",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &.{
                        .Integer = 5,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with float",
            .input = "+5.41",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &.{
                        .Float = 5.41,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with float",
            .input = "-2.1234",
            .expected_expression = .{
                .Prefix = .{
                    .expression = &.{
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
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 1 },
                    .right = &.{ .Integer = 1 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse float addition",
            .input = "1.1 + 1.35",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Float = 1.1 },
                    .right = &.{ .Float = 1.35 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse integer subtraction",
            .input = "40 - 22",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 40 },
                    .right = &.{ .Integer = 22 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse float subtraction",
            .input = "40.54 - 22.33",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Float = 40.54 },
                    .right = &.{ .Float = 22.33 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse integer multiplication",
            .input = "5 * 66",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 5 },
                    .right = &.{ .Integer = 66 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse integer mod",
            .input = "33 % 2",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 33 },
                    .right = &.{ .Integer = 2 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should preserve order of operations without parens",
            .input = "3 * 4 / 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{
                        .Infix = .{
                            .left = &.{
                                .Integer = 3,
                            },
                            .right = &.{
                                .Integer = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &.{ .Integer = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in first pos",
            .input = "(3 * 4) / 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{
                        .Infix = .{
                            .left = &.{
                                .Integer = 3,
                            },
                            .right = &.{
                                .Integer = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &.{ .Integer = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in second pos",
            .input = "3 * (4 / 3)",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{
                        .Infix = .{
                            .left = &.{
                                .Integer = 4,
                            },
                            .right = &.{
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
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Float = 5.3 },
                    .right = &.{ .Float = 66.5 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float mod",
            .input = "1.1 % 5.3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Float = 1.1 },
                    .right = &.{ .Float = 5.3 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should parse integer division",
            .input = "6 / 2",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 6 },
                    .right = &.{ .Integer = 2 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse float division",
            .input = "6.55 / 2.413",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Float = 6.55 },
                    .right = &.{ .Float = 2.413 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse integer less than",
            .input = "1 < 5",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 1 },
                    .right = &.{ .Integer = 5 },
                    .operator = .Lt,
                },
            },
        },
        .{
            .description = "should parse integer less than or equal",
            .input = "1 <= 5",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 1 },
                    .right = &.{ .Integer = 5 },
                    .operator = .LtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer greater than",
            .input = "1 > 5",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 1 },
                    .right = &.{ .Integer = 5 },
                    .operator = .Gt,
                },
            },
        },
        .{
            .description = "should parse integer greater than or equal",
            .input = "1 >= 5",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 1 },
                    .right = &.{ .Integer = 5 },
                    .operator = .GtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer equals",
            .input = "3 == 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{ .Integer = 3 },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse integer does not equal",
            .input = "3 != 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{ .Integer = 3 },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse integer bitwise or",
            .input = "3 | 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{ .Integer = 3 },
                    .operator = .Pipe,
                },
            },
        },
        .{
            .description = "should parse integer bitwise and",
            .input = "3 & 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{ .Integer = 3 },
                    .operator = .Ampersand,
                },
            },
        },
        .{
            .description = "should parse integer bitwise xor",
            .input = "3 ^ 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{ .Integer = 3 },
                    .operator = .Caret,
                },
            },
        },
        .{
            .description = "should parse integer shift left",
            .input = "3 << 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{ .Integer = 3 },
                    .operator = .LeftShift,
                },
            },
        },
        .{
            .description = "should parse integer shift right",
            .input = "3 >> 3",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Integer = 3 },
                    .right = &.{ .Integer = 3 },
                    .operator = .RightShift,
                },
            },
        },
        .{
            .description = "should parse boolean equals",
            .input = "true == false",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Boolean = true },
                    .right = &.{ .Boolean = false },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse boolean does not equal",
            .input = "true != false",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Boolean = true },
                    .right = &.{ .Boolean = false },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse boolean logical or",
            .input = "true or false",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Boolean = true },
                    .right = &.{ .Boolean = false },
                    .operator = .Or,
                },
            },
        },
        .{
            .description = "should parse boolean logical and",
            .input = "true and false",
            .expected_expression = .{
                .Infix = .{
                    .left = &.{ .Boolean = true },
                    .right = &.{ .Boolean = false },
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
                    .start = &.{
                        .Integer = 0,
                    },
                    .end = &.{
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
            .expected_expression = .{
                .Range = .{
                    .start = &.{
                        .Infix = .{
                            .operator = .Plus,
                            .left = &.{
                                .Integer = 2,
                            },
                            .right = &.{
                                .Integer = 3,
                            },
                        },
                    },
                    .end = &.{
                        .Infix = .{
                            .operator = .Minus,
                            .left = &.{
                                .Integer = 50,
                            },
                            .right = &.{
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
            .expected_expression = .{
                .Range = .{
                    .start = &.{
                        .Call = .{
                            .args = .{},
                            .function = &.{
                                .Identifier = "start",
                            },
                        },
                    },
                    .end = &.{
                        .Call = .{
                            .args = .{},
                            .function = &.{
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
            .expected_expression = .{
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
            .expected_expression = .{
                .List = .{},
            },
        },
        .{
            .description = "list one line",
            .input =
            \\List{1, 2}
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .Integer = 1 },
                    &.{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list one line trailing comma",
            .input =
            \\List{1, 2,}
            ,
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .Integer = 1 },
                    &.{ .Integer = 2 },
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
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .Integer = 1 },
                    &.{ .Integer = 2 },
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
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .Integer = 1 },
                    &.{ .Integer = 2 },
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
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .Integer = 1 },
                    &.{ .Integer = 2 },
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
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .Integer = 1 },
                    &.{ .Integer = 2 },
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
            .expected_expression = .{
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .Integer = 1 },
                    &.{ .Integer = 2 },
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
                .List = try test_utils.list(*const ast.Expression, arena.allocator(), &.{
                    &.{ .List = try test_utils.list(
                        *const ast.Expression,
                        arena.allocator(),
                        &.{
                            &.{ .Integer = 1 },
                            &.{ .Integer = 2 },
                        },
                    ) },
                    &.{ .List = try test_utils.list(
                        *const ast.Expression,
                        arena.allocator(),
                        &.{
                            &.{ .Integer = 3 },
                            &.{ .Integer = 4 },
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
            .expected_expression = .{
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
            .expected_expression = .{
                .Table = .{},
            },
        },
        .{
            .description = "table one line",
            .input =
            \\Table{"a" = 1, "b" = 2}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, arena.allocator(), &.{
                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 1 } },
                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "table one line trailing comma",
            .input =
            \\Table{"a" = 1, "b" = 2,}
            ,
            .expected_expression = .{
                .Table = try test_utils.list(ast.TablePair, arena.allocator(), &.{
                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 1 } },
                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 2 } },
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
                .Table = try test_utils.list(ast.TablePair, arena.allocator(), &.{
                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 1 } },
                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 2 } },
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
                .Table = try test_utils.list(ast.TablePair, arena.allocator(), &.{
                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 1 } },
                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 2 } },
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
                .Table = try test_utils.list(ast.TablePair, arena.allocator(), &.{
                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 1 } },
                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 2 } },
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
                .Table = try test_utils.list(ast.TablePair, arena.allocator(), &.{
                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 1 } },
                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 2 } },
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
                .Table = try test_utils.list(ast.TablePair, arena.allocator(), &.{
                    .{
                        .key = &.{ .String = "a" },
                        .value = &.{
                            .Table = try test_utils.list(
                                ast.TablePair,
                                arena.allocator(),
                                &.{
                                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 1 } },
                                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 2 } },
                                },
                            ),
                        },
                    },
                    .{
                        .key = &.{ .String = "b" },
                        .value = &.{
                            .Table = try test_utils.list(
                                ast.TablePair,
                                arena.allocator(),
                                &.{
                                    .{ .key = &.{ .String = "a" }, .value = &.{ .Integer = 3 } },
                                    .{ .key = &.{ .String = "b" }, .value = &.{ .Integer = 4 } },
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
            .expected_expression = .{
                .Call = .{
                    .function = &.{
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
            .expected_expression = .{
                .Call = .{
                    .function = &.{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(ast.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &.{ .Integer = 1 } },
                        .{ .Positional = &.{ .Integer = 2 } },
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
                    .function = &.{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(ast.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &.{ .Integer = 1 } },
                        .{ .Positional = &.{ .Integer = 2 } },
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
                    .function = &.{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(ast.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &.{ .Integer = 1 } },
                        .{ .Positional = &.{ .Integer = 2 } },
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
                    .function = &.{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(ast.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &.{ .Integer = 1 } },
                        .{ .Positional = &.{ .Integer = 2 } },
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
                    .function = &.{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(ast.FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &.{ .Integer = 1 } },
                        .{ .Named = .{ .name = "c", .value = &.{ .Integer = 2 } } },
                        .{ .Named = .{ .name = "b", .value = &.{ .Integer = 3 } } },
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
                    .params = try test_utils.list(ast.FunctionParam, arena.allocator(), &.{
                        .{ .Positional = "a" },
                        .{ .Positional = "b" },
                    }),
                    .body = .{
                        .Block = try test_utils.list(ast.Statement, arena.allocator(), &.{
                            .{
                                .Return = &.{
                                    .Infix = .{
                                        .left = &.{ .Identifier = "a" },
                                        .operator = .Plus,
                                        .right = &.{ .Identifier = "b" },
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
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, arena.allocator(), &.{
                        .{ .Positional = "a" },
                        .{ .Positional = "b" },
                    }),
                    .body = .{
                        .Expression = &.{
                            .Infix = .{
                                .left = &.{ .Identifier = "a" },
                                .operator = .Plus,
                                .right = &.{ .Identifier = "b" },
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
            .expected_expression = .{
                .Function = .{
                    .params = try test_utils.list(ast.FunctionParam, arena.allocator(), &.{
                        .{ .Default = .{ .name = "a", .value = &.{ .Integer = 11 } } },
                        .{ .Default = .{ .name = "b", .value = &.{ .Integer = 12 } } },
                    }),
                    .body = .{
                        .Expression = &.{
                            .Infix = .{
                                .left = &.{ .Identifier = "a" },
                                .operator = .Plus,
                                .right = &.{ .Identifier = "b" },
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
                        .Multiple = try test_utils.list(ast.MatchArm, arena.allocator(), &.{
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
        expected: ast.Program,
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
            .expected = try test_utils.list(ast.Statement, arena.allocator(), &.{
                .{
                    .VarDeclaration = .{
                        .name = "fib",
                        .expression = &.{
                            .Function = .{
                                .name = "fib",
                                .params = try test_utils.list(ast.FunctionParam, arena.allocator(), &.{
                                    .{ .Positional = "n" },
                                }),
                                .body = .{
                                    .Block = try test_utils.list(ast.Statement, arena.allocator(), &.{
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
                                                .body = try test_utils.list(ast.Statement, arena.allocator(), &.{
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
                            .args = try test_utils.list(ast.FunctionArg, arena.allocator(), &.{
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
