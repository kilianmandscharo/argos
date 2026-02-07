const std = @import("std");
const expect = std.testing.expect;

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;
const Expression = parser_module.Expression;
const PrefixExpression = parser_module.PrefixExpression;
const InfixExpression = parser_module.InfixExpression;
const IfExpression = parser_module.IfExpression;
const RangeExpression = parser_module.RangeExpression;
const ForExpression = parser_module.ForExpression;
const CallExpression = parser_module.CallExpression;
const FunctionLiteral = parser_module.FunctionLiteral;
const FunctionParam = parser_module.FunctionParam;
const BlockExpression = parser_module.BlockExpression;
const IndexExpression = parser_module.IndexExpression;
const AssignmentExpression = parser_module.AssignmentExpression;
const Operator = parser_module.Operator;
const ParserError = parser_module.ParserError;

const test_utils = @import("test_utils.zig");
const runTests = test_utils.runTests;
const list = test_utils.list;

// TODO: add error cases

const ExpressionTestCase = struct {
    description: []const u8,
    input: []const u8,
    expected_expression: Expression,
    expected_error: ?ParserError = null,
};

const runExpressionTest = struct {
    fn runTest(arena: std.mem.Allocator, test_case: ExpressionTestCase) anyerror!void {
        const expression = try getExpression(arena, test_case.input);
        return expectExpression(test_case.expected_expression, expression.*);
    }
}.runTest;

fn getProgram(arena: std.mem.Allocator, content: []const u8) !Expression {
    var lexer = try Lexer.init(arena, content);
    var parser = try Parser.init(&lexer, .{ .arena = arena, .debug = false });
    return try parser.parseProgram();
}

fn getExpression(arena: std.mem.Allocator, content: []const u8) !*const Expression {
    const program = try getProgram(arena, content);
    if (program != .Program) {
        return error.ExpectedProgram;
    }
    try expect(program.Program.items.len >= 1);
    const statement = program.Program.items[0];
    if (statement.* != .Statement) {
        return error.ExpectedStatement;
    }
    return statement.Statement;
}

fn expectExpression(expected: Expression, actual: Expression) !void {
    const Tag = std.meta.Tag(@TypeOf(expected));

    const expectedTag = @as(Tag, expected);
    const actualTag = @as(Tag, actual);

    try std.testing.expectEqual(expectedTag, actualTag);

    switch (expected) {
        .Program => |program| {
            try std.testing.expectEqual(program.items.len, actual.Program.items.len);
            for (program.items, 0..) |item, i| {
                try expectExpression(item.*, actual.Program.items[i].*);
            }
        },
        .Statement => |statement| {
            try expectExpression(statement.*, actual.Statement.*);
        },
        .Identifier => |ident| try std.testing.expectEqualStrings(ident, actual.Identifier),
        .StringLiteral => |string| try std.testing.expectEqualStrings(string, actual.StringLiteral),
        .PrefixExpression => |prefix_expression| {
            try std.testing.expectEqual(prefix_expression.operator, actual.PrefixExpression.operator);
            try expectExpression(prefix_expression.expression.*, actual.PrefixExpression.expression.*);
        },
        .InfixExpression => |infix_expression| {
            try std.testing.expectEqual(infix_expression.operator, actual.InfixExpression.operator);
            try expectExpression(infix_expression.left.*, actual.InfixExpression.left.*);
            try expectExpression(infix_expression.right.*, actual.InfixExpression.right.*);
        },
        .FunctionLiteral => |func_literal| {
            try std.testing.expectEqual(func_literal.params.items.len, actual.FunctionLiteral.params.items.len);
            try expectExpression(func_literal.body.*, actual.FunctionLiteral.body.*);
        },
        .IfExpression => |if_expression| {
            try expectExpression(if_expression.condition.*, actual.IfExpression.condition.*);
            try expectExpression(Expression{ .BlockExpression = if_expression.body }, Expression{ .BlockExpression = actual.IfExpression.body });
            if (if_expression.alternative) |alternative| {
                try expectExpression(Expression{ .BlockExpression = alternative }, Expression{ .BlockExpression = actual.IfExpression.alternative.? });
            }
        },
        .RangeExpression => |range_expression| {
            try expectExpression(range_expression.left.*, actual.RangeExpression.left.*);
            try expectExpression(range_expression.right.*, actual.RangeExpression.right.*);
        },
        .CallExpression => |call_expression| {
            try expectExpression(call_expression.function.*, actual.CallExpression.function.*);
            try std.testing.expectEqual(call_expression.args.items.len, actual.CallExpression.args.items.len);
            for (call_expression.args.items, 0..) |expression, i| {
                try expectExpression(expression.*, actual.CallExpression.args.items[i].*);
            }
        },
        .ForExpression => |for_expression| {
            try std.testing.expectEqualStrings(for_expression.variable, actual.ForExpression.variable);
            try expectExpression(Expression{ .RangeExpression = for_expression.range }, Expression{ .RangeExpression = actual.ForExpression.range });
            try expectExpression(Expression{ .BlockExpression = for_expression.body }, Expression{ .BlockExpression = actual.ForExpression.body });
        },
        .ArrayLiteral => |array_literal| {
            try std.testing.expectEqual(array_literal.items.len, actual.ArrayLiteral.items.len);
            for (array_literal.items, 0..) |item, i| {
                try expectExpression(item.*, actual.ArrayLiteral.items[i].*);
            }
        },
        .TableLiteral => |table_literal| {
            try std.testing.expectEqual(table_literal.items.len, actual.TableLiteral.items.len);
            for (table_literal.items, 0..) |item, i| {
                try expectExpression(item.*, actual.TableLiteral.items[i].*);
            }
        },
        .BlockExpression => |block_expression| {
            try std.testing.expectEqual(block_expression.expressions.items.len, actual.BlockExpression.expressions.items.len);
            for (block_expression.expressions.items, 0..) |expression, i| {
                try expectExpression(expression.*, actual.BlockExpression.expressions.items[i].*);
            }
        },
        .AssignmentExpression => |assignment_expression| {
            try expectExpression(assignment_expression.left.*, actual.AssignmentExpression.left.*);
            try expectExpression(assignment_expression.expression.*, actual.AssignmentExpression.expression.*);
        },
        .ReturnExpression => |return_expression| {
            try expectExpression(return_expression.*, actual.ReturnExpression.*);
        },
        .IndexExpression => |index_expression| {
            try expectExpression(index_expression.left.*, actual.IndexExpression.left.*);
            try expectExpression(index_expression.index_expression.*, actual.IndexExpression.index_expression.*);
        },
        else => try std.testing.expectEqual(expected, actual),
    }
}

test "parse expressions" {
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
            .expected_expression = Expression{ .StringLiteral = "foo" },
        },
        .{
            .description = "should parse integer literal",
            .input =
            \\666
            ,
            .expected_expression = Expression{ .IntegerLiteral = 666 },
        },
        .{
            .description = "should parse float literal",
            .input =
            \\3.1415
            ,
            .expected_expression = Expression{ .FloatLiteral = 3.1415 },
        },
        .{
            .description = "should parse true",
            .input =
            \\true
            ,
            .expected_expression = Expression{ .BooleanLiteral = true },
        },
        .{
            .description = "should parse false",
            .input =
            \\false
            ,
            .expected_expression = Expression{ .BooleanLiteral = false },
        },
    };

    try runTests(ExpressionTestCase, "parse expressions", &test_cases, runExpressionTest);
}

test "should parse prefix expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse bang operator with true",
            .input = "!true",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .BooleanLiteral = true,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse bang operator with false",
            .input = "!false",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .BooleanLiteral = false,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse tilde operator with false",
            .input = "~false",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .BooleanLiteral = false,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with integer",
            .input = "+5",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .IntegerLiteral = 5,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with integer",
            .input = "-2",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .IntegerLiteral = 2,
                    },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse tilde operator with integer",
            .input = "~5",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .IntegerLiteral = 5,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with float",
            .input = "+5.41",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .FloatLiteral = 5.41,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with float",
            .input = "-2.1234",
            .expected_expression = Expression{
                .PrefixExpression = PrefixExpression{
                    .expression = &Expression{
                        .FloatLiteral = 2.1234,
                    },
                    .operator = .Minus,
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse prefix expression", &test_cases, runExpressionTest);
}

test "should parse infix expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse integer addition",
            .input = "1 + 1",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 1 },
                    .right = &Expression{ .IntegerLiteral = 1 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse float addition",
            .input = "1.1 + 1.35",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .FloatLiteral = 1.1 },
                    .right = &Expression{ .FloatLiteral = 1.35 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse integer subtraction",
            .input = "40 - 22",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 40 },
                    .right = &Expression{ .IntegerLiteral = 22 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse float subtraction",
            .input = "40.54 - 22.33",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .FloatLiteral = 40.54 },
                    .right = &Expression{ .FloatLiteral = 22.33 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse integer multiplication",
            .input = "5 * 66",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 5 },
                    .right = &Expression{ .IntegerLiteral = 66 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse integer mod",
            .input = "33 % 2",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 33 },
                    .right = &Expression{ .IntegerLiteral = 2 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should preserve order of operations without parens",
            .input = "3 * 4 / 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{
                        .InfixExpression = InfixExpression{
                            .left = &Expression{
                                .IntegerLiteral = 3,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in first pos",
            .input = "(3 * 4) / 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{
                        .InfixExpression = InfixExpression{
                            .left = &Expression{
                                .IntegerLiteral = 3,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in second pos",
            .input = "3 * (4 / 3)",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{
                        .InfixExpression = InfixExpression{
                            .left = &Expression{
                                .IntegerLiteral = 4,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 3,
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
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .FloatLiteral = 5.3 },
                    .right = &Expression{ .FloatLiteral = 66.5 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float mod",
            .input = "1.1 % 5.3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .FloatLiteral = 1.1 },
                    .right = &Expression{ .FloatLiteral = 5.3 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should parse integer division",
            .input = "6 / 2",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 6 },
                    .right = &Expression{ .IntegerLiteral = 2 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse float division",
            .input = "6.55 / 2.413",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .FloatLiteral = 6.55 },
                    .right = &Expression{ .FloatLiteral = 2.413 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse integer less than",
            .input = "1 < 5",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 1 },
                    .right = &Expression{ .IntegerLiteral = 5 },
                    .operator = .Lt,
                },
            },
        },
        .{
            .description = "should parse integer less than or equal",
            .input = "1 <= 5",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 1 },
                    .right = &Expression{ .IntegerLiteral = 5 },
                    .operator = .LtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer greater than",
            .input = "1 > 5",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 1 },
                    .right = &Expression{ .IntegerLiteral = 5 },
                    .operator = .Gt,
                },
            },
        },
        .{
            .description = "should parse integer greater than or equal",
            .input = "1 >= 5",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 1 },
                    .right = &Expression{ .IntegerLiteral = 5 },
                    .operator = .GtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer equals",
            .input = "3 == 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse integer does not equal",
            .input = "3 != 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse integer bitwise or",
            .input = "3 | 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .Pipe,
                },
            },
        },
        .{
            .description = "should parse integer bitwise and",
            .input = "3 & 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .Ampersand,
                },
            },
        },
        .{
            .description = "should parse integer bitwise xor",
            .input = "3 ^ 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .Caret,
                },
            },
        },
        .{
            .description = "should parse integer shift left",
            .input = "3 << 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .LeftShift,
                },
            },
        },
        .{
            .description = "should parse integer shift right",
            .input = "3 >> 3",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .IntegerLiteral = 3 },
                    .right = &Expression{ .IntegerLiteral = 3 },
                    .operator = .RightShift,
                },
            },
        },
        .{
            .description = "should parse boolean equals",
            .input = "true == false",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .BooleanLiteral = true },
                    .right = &Expression{ .BooleanLiteral = false },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse boolean does not equal",
            .input = "true != false",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .BooleanLiteral = true },
                    .right = &Expression{ .BooleanLiteral = false },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse boolean logical or",
            .input = "true or false",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .BooleanLiteral = true },
                    .right = &Expression{ .BooleanLiteral = false },
                    .operator = .Or,
                },
            },
        },
        .{
            .description = "should parse boolean logical and",
            .input = "true and false",
            .expected_expression = Expression{
                .InfixExpression = InfixExpression{
                    .left = &Expression{ .BooleanLiteral = true },
                    .right = &Expression{ .BooleanLiteral = false },
                    .operator = .And,
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse infix expression", &test_cases, runExpressionTest);
}

test "function declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty function one line",
            .input =
            \\() -> {}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = .{},
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = .{},
                        },
                    },
                },
            },
        },
        .{
            .description = "empty function multi lines",
            .input =
            \\() -> {
            \\
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = .{},
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = .{},
                        },
                    },
                },
            },
        },
        .{
            .description = "one liner with params",
            .input =
            \\(a, b) -> { return a + b }
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "a" },
                        FunctionParam{ .Identifier = "b" },
                    }),
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .ReturnExpression = &Expression{
                                            .InfixExpression = InfixExpression{
                                                .operator = .Plus,
                                                .left = &Expression{
                                                    .Identifier = "a",
                                                },
                                                .right = &Expression{
                                                    .Identifier = "b",
                                                },
                                            },
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "multiple lines with params",
            .input =
            \\(a, b) -> {
            \\    return a + b
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "a" },
                        FunctionParam{ .Identifier = "b" },
                    }),
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .ReturnExpression = &Expression{
                                            .InfixExpression = InfixExpression{
                                                .operator = .Plus,
                                                .left = &Expression{
                                                    .Identifier = "a",
                                                },
                                                .right = &Expression{
                                                    .Identifier = "b",
                                                },
                                            },
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "multi liner with params and multiple statements",
            .input =
            \\(a, b) -> {
            \\    x = a * b
            \\    y = a / b
            \\    return x + y
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "a" },
                        FunctionParam{ .Identifier = "b" },
                    }),
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .AssignmentExpression = AssignmentExpression{
                                            .left = &Expression{
                                                .Identifier = "x",
                                            },
                                            .expression = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .operator = .Asterisk,
                                                    .left = &Expression{
                                                        .Identifier = "a",
                                                    },
                                                    .right = &Expression{
                                                        .Identifier = "b",
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .AssignmentExpression = AssignmentExpression{
                                            .left = &Expression{
                                                .Identifier = "y",
                                            },
                                            .expression = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .operator = .Slash,
                                                    .left = &Expression{
                                                        .Identifier = "a",
                                                    },
                                                    .right = &Expression{
                                                        .Identifier = "b",
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .ReturnExpression = &Expression{
                                            .InfixExpression = InfixExpression{
                                                .operator = .Plus,
                                                .left = &Expression{
                                                    .Identifier = "x",
                                                },
                                                .right = &Expression{
                                                    .Identifier = "y",
                                                },
                                            },
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "multi liner with gaps",
            .input =
            \\(a, b) -> {
            \\
            \\    x = a * b
            \\
            \\    y = a / b
            \\
            \\    return x + y
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "a" },
                        FunctionParam{ .Identifier = "b" },
                    }),
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .AssignmentExpression = AssignmentExpression{
                                            .left = &Expression{
                                                .Identifier = "x",
                                            },
                                            .expression = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .operator = .Asterisk,
                                                    .left = &Expression{
                                                        .Identifier = "a",
                                                    },
                                                    .right = &Expression{
                                                        .Identifier = "b",
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .AssignmentExpression = AssignmentExpression{
                                            .left = &Expression{
                                                .Identifier = "y",
                                            },
                                            .expression = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .operator = .Slash,
                                                    .left = &Expression{
                                                        .Identifier = "a",
                                                    },
                                                    .right = &Expression{
                                                        .Identifier = "b",
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .ReturnExpression = &Expression{
                                            .InfixExpression = InfixExpression{
                                                .operator = .Plus,
                                                .left = &Expression{
                                                    .Identifier = "x",
                                                },
                                                .right = &Expression{
                                                    .Identifier = "y",
                                                },
                                            },
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "one liner without return",
            .input =
            \\(a, b) -> { a + b }
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "a" },
                        FunctionParam{ .Identifier = "b" },
                    }),
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .InfixExpression = InfixExpression{
                                            .operator = .Plus,
                                            .left = &Expression{
                                                .Identifier = "a",
                                            },
                                            .right = &Expression{
                                                .Identifier = "b",
                                            },
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "multi liner without return",
            .input =
            \\(a, b) -> {
            \\    x = a * b
            \\    y = a / b
            \\    x + y
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "a" },
                        FunctionParam{ .Identifier = "b" },
                    }),
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .AssignmentExpression = AssignmentExpression{
                                            .left = &Expression{
                                                .Identifier = "x",
                                            },
                                            .expression = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .operator = .Asterisk,
                                                    .left = &Expression{
                                                        .Identifier = "a",
                                                    },
                                                    .right = &Expression{
                                                        .Identifier = "b",
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .AssignmentExpression = AssignmentExpression{
                                            .left = &Expression{
                                                .Identifier = "y",
                                            },
                                            .expression = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .operator = .Slash,
                                                    .left = &Expression{
                                                        .Identifier = "a",
                                                    },
                                                    .right = &Expression{
                                                        .Identifier = "b",
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .InfixExpression = InfixExpression{
                                            .operator = .Plus,
                                            .left = &Expression{
                                                .Identifier = "x",
                                            },
                                            .right = &Expression{
                                                .Identifier = "y",
                                            },
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "more complex return expression",
            .input =
            \\() -> {
            \\    array = [1, 2]
            \\    return array[0] + array[1]
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = .{},
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .AssignmentExpression = AssignmentExpression{
                                            .left = &Expression{
                                                .Identifier = "array",
                                            },
                                            .expression = &Expression{
                                                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                                                    &Expression{
                                                        .IntegerLiteral = 1,
                                                    },
                                                    &Expression{
                                                        .IntegerLiteral = 2,
                                                    },
                                                }),
                                            },
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .ReturnExpression = &Expression{
                                            .InfixExpression = InfixExpression{
                                                .left = &Expression{
                                                    .IndexExpression = IndexExpression{
                                                        .left = &Expression{
                                                            .Identifier = "array",
                                                        },
                                                        .index_expression = &Expression{
                                                            .IntegerLiteral = 0,
                                                        },
                                                    },
                                                },
                                                .right = &Expression{
                                                    .IndexExpression = IndexExpression{
                                                        .left = &Expression{
                                                            .Identifier = "array",
                                                        },
                                                        .index_expression = &Expression{
                                                            .IntegerLiteral = 1,
                                                        },
                                                    },
                                                },
                                                .operator = .Plus,
                                            },
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "with if expression",
            .input =
            \\(n) -> {
            \\    if n == 0 {
            \\        return 1
            \\    }
            \\    if n == 1 { return 1 }
            \\    return 0
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "n" },
                    }),
                    .body = &Expression{
                        .BlockExpression = BlockExpression{
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .Statement = &Expression{
                                        .IfExpression = IfExpression{
                                            .condition = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .left = &Expression{ .Identifier = "n" },
                                                    .right = &Expression{ .IntegerLiteral = 0 },
                                                    .operator = .Eq,
                                                },
                                            },
                                            .body = BlockExpression{
                                                .expressions = try list(*const Expression, arena.allocator(), &.{
                                                    &Expression{
                                                        .Statement = &Expression{
                                                            .ReturnExpression = &Expression{
                                                                .IntegerLiteral = 1,
                                                            },
                                                        },
                                                    },
                                                }),
                                            },
                                            .alternative = null,
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .IfExpression = IfExpression{
                                            .condition = &Expression{
                                                .InfixExpression = InfixExpression{
                                                    .left = &Expression{ .Identifier = "n" },
                                                    .right = &Expression{ .IntegerLiteral = 1 },
                                                    .operator = .Eq,
                                                },
                                            },
                                            .body = BlockExpression{
                                                .expressions = try list(*const Expression, arena.allocator(), &.{
                                                    &Expression{
                                                        .Statement = &Expression{
                                                            .ReturnExpression = &Expression{
                                                                .IntegerLiteral = 1,
                                                            },
                                                        },
                                                    },
                                                }),
                                            },
                                            .alternative = null,
                                        },
                                    },
                                },
                                &Expression{
                                    .Statement = &Expression{
                                        .ReturnExpression = &Expression{
                                            .IntegerLiteral = 0,
                                        },
                                    },
                                },
                            }),
                        },
                    },
                },
            },
        },
        .{
            .description = "one liner with params and no braces",
            .input =
            \\(a, b) -> a + b
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{ .Identifier = "a" },
                        FunctionParam{ .Identifier = "b" },
                    }),
                    .body = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Plus,
                            .left = &Expression{
                                .Identifier = "a",
                            },
                            .right = &Expression{
                                .Identifier = "b",
                            },
                        },
                    },
                },
            },
        },
        .{
            .description = "with default params",
            .input =
            \\(a = 1, b = 2) -> a + b
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .params = try list(FunctionParam, arena.allocator(), &.{
                        FunctionParam{
                            .AssignmentExpression = AssignmentExpression{
                                .expression = &Expression{ .IntegerLiteral = 1 },
                                .left = &Expression{ .Identifier = "a" },
                            },
                        },
                        FunctionParam{
                            .AssignmentExpression = AssignmentExpression{
                                .expression = &Expression{ .IntegerLiteral = 2 },
                                .left = &Expression{ .Identifier = "b" },
                            },
                        },
                    }),
                    .body = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Plus,
                            .left = &Expression{
                                .Identifier = "a",
                            },
                            .right = &Expression{
                                .Identifier = "b",
                            },
                        },
                    },
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse function declaration", &test_cases, runExpressionTest);
}

test "if expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "single line empty no else",
            .input =
            \\if 5 == 5 {}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = .{},
                    },
                    .alternative = null,
                },
            },
        },
        .{
            .description = "single line empty with else",
            .input =
            \\if 5 == 5 {} else {}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = .{},
                    },
                    .alternative = BlockExpression{
                        .expressions = .{},
                    },
                },
            },
        },
        .{
            .description = "single line no else",
            .input =
            \\if 5 == 5 { 3 + 2 }
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .Statement = &Expression{
                                    .InfixExpression = InfixExpression{
                                        .operator = .Plus,
                                        .left = &Expression{
                                            .IntegerLiteral = 3,
                                        },
                                        .right = &Expression{
                                            .IntegerLiteral = 2,
                                        },
                                    },
                                },
                            },
                        }),
                    },
                    .alternative = null,
                },
            },
        },
        .{
            .description = "single line with else",
            .input =
            \\if 5 == 5 { 3 + 2 } else { 1 * 1 }
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .Statement = &Expression{
                                    .InfixExpression = InfixExpression{
                                        .operator = .Plus,
                                        .left = &Expression{
                                            .IntegerLiteral = 3,
                                        },
                                        .right = &Expression{
                                            .IntegerLiteral = 2,
                                        },
                                    },
                                },
                            },
                        }),
                    },
                    .alternative = BlockExpression{
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .Statement = &Expression{
                                    .InfixExpression = InfixExpression{
                                        .operator = .Asterisk,
                                        .left = &Expression{
                                            .IntegerLiteral = 1,
                                        },
                                        .right = &Expression{
                                            .IntegerLiteral = 1,
                                        },
                                    },
                                },
                            },
                        }),
                    },
                },
            },
        },
        .{
            .description = "multi line empty no else with empty line",
            .input =
            \\if 5 == 5 {
            \\
            \\}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = .{},
                    },
                    .alternative = null,
                },
            },
        },
        .{
            .description = "multi line empty no else",
            .input =
            \\if 5 == 5 {
            \\}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = .{},
                    },
                    .alternative = null,
                },
            },
        },
        .{
            .description = "multi line empty with else",
            .input =
            \\if 5 == 5 {
            \\} else {
            \\}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = .{},
                    },
                    .alternative = BlockExpression{
                        .expressions = .{},
                    },
                },
            },
        },
        .{
            .description = "multi line no else",
            .input =
            \\if 5 == 5 {
            \\    3 + 2
            \\}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .Statement = &Expression{
                                    .InfixExpression = InfixExpression{
                                        .operator = .Plus,
                                        .left = &Expression{
                                            .IntegerLiteral = 3,
                                        },
                                        .right = &Expression{
                                            .IntegerLiteral = 2,
                                        },
                                    },
                                },
                            },
                        }),
                    },
                    .alternative = null,
                },
            },
        },
        .{
            .description = "multi line with else",
            .input =
            \\if 5 == 5 {
            \\    3 + 2
            \\} else {
            \\    1 * 1
            \\}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .condition = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Eq,
                            .left = &Expression{
                                .IntegerLiteral = 5,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                    .body = BlockExpression{
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .Statement = &Expression{
                                    .InfixExpression = InfixExpression{
                                        .operator = .Plus,
                                        .left = &Expression{
                                            .IntegerLiteral = 3,
                                        },
                                        .right = &Expression{
                                            .IntegerLiteral = 2,
                                        },
                                    },
                                },
                            },
                        }),
                    },
                    .alternative = BlockExpression{
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .Statement = &Expression{
                                    .InfixExpression = InfixExpression{
                                        .operator = .Asterisk,
                                        .left = &Expression{
                                            .IntegerLiteral = 1,
                                        },
                                        .right = &Expression{
                                            .IntegerLiteral = 1,
                                        },
                                    },
                                },
                            },
                        }),
                    },
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse if expression", &test_cases, runExpressionTest);
}

test "range expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "two integers",
            .input =
            \\0..10
            ,
            .expected_expression = Expression{
                .RangeExpression = RangeExpression{
                    .left = &Expression{
                        .IntegerLiteral = 0,
                    },
                    .right = &Expression{
                        .IntegerLiteral = 10,
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
                .RangeExpression = RangeExpression{
                    .left = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Plus,
                            .left = &Expression{
                                .IntegerLiteral = 2,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 3,
                            },
                        },
                    },
                    .right = &Expression{
                        .InfixExpression = InfixExpression{
                            .operator = .Minus,
                            .left = &Expression{
                                .IntegerLiteral = 50,
                            },
                            .right = &Expression{
                                .IntegerLiteral = 10,
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
                .RangeExpression = RangeExpression{
                    .left = &Expression{
                        .CallExpression = CallExpression{
                            .args = .{},
                            .function = &Expression{
                                .Identifier = "start",
                            },
                        },
                    },
                    .right = &Expression{
                        .CallExpression = CallExpression{
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

    try runTests(ExpressionTestCase, "parse range expression", &test_cases, runExpressionTest);
}

test "for expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "simple for loop",
            .input =
            \\for i in 0..10 {
            \\    x = x * i
            \\}
            ,
            .expected_expression = Expression{
                .ForExpression = ForExpression{
                    .variable = "i",
                    .range = RangeExpression{
                        .left = &Expression{
                            .IntegerLiteral = 0,
                        },
                        .right = &Expression{
                            .IntegerLiteral = 10,
                        },
                    },
                    .body = BlockExpression{
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .Statement = &Expression{
                                    .AssignmentExpression = AssignmentExpression{
                                        .left = &Expression{
                                            .Identifier = "x",
                                        },
                                        .expression = &Expression{
                                            .InfixExpression = InfixExpression{
                                                .operator = .Asterisk,
                                                .left = &Expression{
                                                    .Identifier = "x",
                                                },
                                                .right = &Expression{
                                                    .Identifier = "i",
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        }),
                    },
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse for expression", &test_cases, runExpressionTest);
}

test "function call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "function call one line no trailing comma",
            .input =
            \\test(1, 2)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
        },
        .{
            .description = "function call one line with trailing comma",
            .input =
            \\test(1, 2,)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
        },
        .{
            .description = "function call multiline no trailing comma",
            .input =
            \\test(
            \\    1,
            \\    2
            \\)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
        },
        .{
            .description = "function call multiline with trailing comma",
            .input =
            \\test(
            \\    1,
            \\    2,
            \\)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
        },
        .{
            .description = "iife",
            .input =
            \\(a, b) -> {
            \\    return a + b
            \\}(1, 2)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .FunctionLiteral = FunctionLiteral{
                            .params = try list(FunctionParam, arena.allocator(), &.{
                                FunctionParam{ .Identifier = "a" },
                                FunctionParam{ .Identifier = "b" },
                            }),
                            .body = &Expression{
                                .BlockExpression = BlockExpression{
                                    .expressions = try list(*const Expression, arena.allocator(), &.{
                                        &Expression{
                                            .Statement = &Expression{
                                                .ReturnExpression = &Expression{
                                                    .InfixExpression = InfixExpression{
                                                        .operator = .Plus,
                                                        .left = &Expression{
                                                            .Identifier = "a",
                                                        },
                                                        .right = &Expression{
                                                            .Identifier = "b",
                                                        },
                                                    },
                                                },
                                            },
                                        },
                                    }),
                                },
                            },
                        },
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
        },
        .{
            .description = "one liner iife",
            .input =
            \\(a, b) -> { return a + b }(1, 2)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .FunctionLiteral = FunctionLiteral{
                            .params = try list(FunctionParam, arena.allocator(), &.{
                                FunctionParam{ .Identifier = "a" },
                                FunctionParam{ .Identifier = "b" },
                            }),
                            .body = &Expression{
                                .BlockExpression = BlockExpression{
                                    .expressions = try list(*const Expression, arena.allocator(), &.{
                                        &Expression{
                                            .Statement = &Expression{
                                                .ReturnExpression = &Expression{
                                                    .InfixExpression = InfixExpression{
                                                        .operator = .Plus,
                                                        .left = &Expression{
                                                            .Identifier = "a",
                                                        },
                                                        .right = &Expression{
                                                            .Identifier = "b",
                                                        },
                                                    },
                                                },
                                            },
                                        },
                                    }),
                                },
                            },
                        },
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse function call", &test_cases, runExpressionTest);
}

test "array literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "integer array one line no trailing comma no element",
            .input =
            \\[]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{}),
            },
        },
        .{
            .description = "integer array one line no trailing comma one element",
            .input =
            \\[1]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .IntegerLiteral = 1,
                    },
                }),
            },
        },
        .{
            .description = "integer array one line no trailing comma",
            .input =
            \\[1, 2, 3, 4, 5]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .IntegerLiteral = 1,
                    },
                    &Expression{
                        .IntegerLiteral = 2,
                    },
                    &Expression{
                        .IntegerLiteral = 3,
                    },
                    &Expression{
                        .IntegerLiteral = 4,
                    },
                    &Expression{
                        .IntegerLiteral = 5,
                    },
                }),
            },
        },
        .{
            .description = "integer array one line with trailing comma",
            .input =
            \\[1, 2, 3, 4, 5,]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .IntegerLiteral = 1,
                    },
                    &Expression{
                        .IntegerLiteral = 2,
                    },
                    &Expression{
                        .IntegerLiteral = 3,
                    },
                    &Expression{
                        .IntegerLiteral = 4,
                    },
                    &Expression{
                        .IntegerLiteral = 5,
                    },
                }),
            },
        },
        .{
            .description = "integer array multi line no trailing comma",
            .input =
            \\[
            \\    1,
            \\    2,
            \\    3,
            \\    4,
            \\    5
            \\]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .IntegerLiteral = 1,
                    },
                    &Expression{
                        .IntegerLiteral = 2,
                    },
                    &Expression{
                        .IntegerLiteral = 3,
                    },
                    &Expression{
                        .IntegerLiteral = 4,
                    },
                    &Expression{
                        .IntegerLiteral = 5,
                    },
                }),
            },
        },
        .{
            .description = "integer array multi line with trailing comma",
            .input =
            \\[
            \\    1,
            \\    2,
            \\    3,
            \\    4,
            \\    5,
            \\]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .IntegerLiteral = 1,
                    },
                    &Expression{
                        .IntegerLiteral = 2,
                    },
                    &Expression{
                        .IntegerLiteral = 3,
                    },
                    &Expression{
                        .IntegerLiteral = 4,
                    },
                    &Expression{
                        .IntegerLiteral = 5,
                    },
                }),
            },
        },
        .{
            .description = "array of arrays",
            .input =
            \\[
            \\    [1, 2],
            \\    [3, 4]
            \\]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .IntegerLiteral = 1,
                            },
                            &Expression{
                                .IntegerLiteral = 2,
                            },
                        }),
                    },
                    &Expression{
                        .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .IntegerLiteral = 3,
                            },
                            &Expression{
                                .IntegerLiteral = 4,
                            },
                        }),
                    },
                }),
            },
        },
        .{
            .description = "array of tables",
            .input =
            \\[
            \\    { x = 11 },
            \\    { x = 21 },
            \\]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .left = &Expression{
                                        .Identifier = "x",
                                    },
                                    .expression = &Expression{
                                        .IntegerLiteral = 11,
                                    },
                                },
                            },
                        }),
                    },
                    &Expression{
                        .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .left = &Expression{
                                        .Identifier = "x",
                                    },
                                    .expression = &Expression{
                                        .IntegerLiteral = 21,
                                    },
                                },
                            },
                        }),
                    },
                }),
            },
        },
    };

    try runTests(ExpressionTestCase, "parse array literal", &test_cases, runExpressionTest);
}

test "assignment expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "assignment expression",
            .input =
            \\x = 5
            ,
            .expected_expression = Expression{
                .AssignmentExpression = AssignmentExpression{
                    .left = &Expression{
                        .Identifier = "x",
                    },
                    .expression = &Expression{
                        .IntegerLiteral = 5,
                    },
                },
            },
        },
        .{
            .description = "assign to table",
            .input =
            \\table["a"] = 1
            ,
            .expected_expression = Expression{
                .AssignmentExpression = AssignmentExpression{
                    .left = &Expression{
                        .IndexExpression = IndexExpression{
                            .left = &Expression{
                                .Identifier = "table",
                            },
                            .index_expression = &Expression{
                                .StringLiteral = "a",
                            },
                        },
                    },
                    .expression = &Expression{
                        .IntegerLiteral = 1,
                    },
                },
            },
        },
        .{
            .description = "assign to array",
            .input =
            \\array[0] = 1
            ,
            .expected_expression = Expression{
                .AssignmentExpression = AssignmentExpression{
                    .left = &Expression{
                        .IndexExpression = IndexExpression{
                            .left = &Expression{
                                .Identifier = "array",
                            },
                            .index_expression = &Expression{
                                .IntegerLiteral = 0,
                            },
                        },
                    },
                    .expression = &Expression{
                        .IntegerLiteral = 1,
                    },
                },
            },
        },
        .{
            .description = "nested assignment expression",
            .input =
            \\x = y = 5
            ,
            .expected_expression = Expression{
                .AssignmentExpression = AssignmentExpression{
                    .left = &Expression{
                        .Identifier = "x",
                    },
                    .expression = &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "y",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse assignment expression", &test_cases, runExpressionTest);
}

test "table expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty table",
            .input =
            \\{}
            ,
            .expected_expression = Expression{
                .TableLiteral = .{},
            },
        },
        .{
            .description = "table expression one line with comma",
            .input =
            \\{ x = 1, }
            ,
            .expected_expression = Expression{
                .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "x",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                }),
            },
        },
        .{
            .description = "table expression one line without comma",
            .input =
            \\{ x = 1 }
            ,
            .expected_expression = Expression{
                .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "x",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                }),
            },
        },
        .{
            .description = "table expression one line with comma multiple entries",
            .input =
            \\{ x = 1, y = 2, }
            ,
            .expected_expression = Expression{
                .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "x",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "y",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 2,
                            },
                        },
                    },
                }),
            },
        },
        .{
            .description = "table expression one line without comma multiple entries",
            .input =
            \\{ x = 1, y = 2 }
            ,
            .expected_expression = Expression{
                .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "x",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "y",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 2,
                            },
                        },
                    },
                }),
            },
        },
        .{
            .description = "table expression multi line with trailing comma",
            .input =
            \\{
            \\     x = 1,
            \\     y = 2,
            \\     z = x * y,
            \\}
            ,
            .expected_expression = Expression{
                .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "x",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "y",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 2,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "z",
                            },
                            .expression = &Expression{
                                .InfixExpression = InfixExpression{
                                    .operator = .Asterisk,
                                    .left = &Expression{
                                        .Identifier = "x",
                                    },
                                    .right = &Expression{
                                        .Identifier = "y",
                                    },
                                },
                            },
                        },
                    },
                }),
            },
        },
        .{
            .description = "table expression multi line without trailing comma",
            .input =
            \\{
            \\     x = 1,
            \\     y = 2,
            \\     z = x * y
            \\}
            ,
            .expected_expression = Expression{
                .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "x",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "y",
                            },
                            .expression = &Expression{
                                .IntegerLiteral = 2,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .left = &Expression{
                                .Identifier = "z",
                            },
                            .expression = &Expression{
                                .InfixExpression = InfixExpression{
                                    .operator = .Asterisk,
                                    .left = &Expression{
                                        .Identifier = "x",
                                    },
                                    .right = &Expression{
                                        .Identifier = "y",
                                    },
                                },
                            },
                        },
                    },
                }),
            },
        },
    };

    try runTests(ExpressionTestCase, "parse table expression", &test_cases, runExpressionTest);
}

test "index expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "index array literal",
            .input =
            \\[1, 2, 3][0]
            ,
            .expected_expression = Expression{
                .IndexExpression = IndexExpression{
                    .left = &Expression{
                        .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .IntegerLiteral = 1,
                            },
                            &Expression{
                                .IntegerLiteral = 2,
                            },
                            &Expression{
                                .IntegerLiteral = 3,
                            },
                        }),
                    },
                    .index_expression = &Expression{
                        .IntegerLiteral = 0,
                    },
                },
            },
        },
        .{
            .description = "index array identifier",
            .input =
            \\array[0]
            ,
            .expected_expression = Expression{
                .IndexExpression = IndexExpression{
                    .left = &Expression{
                        .Identifier = "array",
                    },
                    .index_expression = &Expression{
                        .IntegerLiteral = 0,
                    },
                },
            },
        },
        .{
            .description = "index table literal",
            .input =
            \\{
            \\    a = 6,
            \\    b = 6,
            \\    c = 6,
            \\}["c"]
            ,
            .expected_expression = Expression{
                .IndexExpression = IndexExpression{
                    .left = &Expression{
                        .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .left = &Expression{
                                        .Identifier = "a",
                                    },
                                    .expression = &Expression{
                                        .IntegerLiteral = 6,
                                    },
                                },
                            },
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .left = &Expression{
                                        .Identifier = "b",
                                    },
                                    .expression = &Expression{
                                        .IntegerLiteral = 6,
                                    },
                                },
                            },
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .left = &Expression{
                                        .Identifier = "c",
                                    },
                                    .expression = &Expression{
                                        .IntegerLiteral = 6,
                                    },
                                },
                            },
                        }),
                    },
                    .index_expression = &Expression{
                        .StringLiteral = "c",
                    },
                },
            },
        },
        .{
            .description = "index table identifier",
            .input =
            \\table["foo"]
            ,
            .expected_expression = Expression{
                .IndexExpression = IndexExpression{
                    .left = &Expression{
                        .Identifier = "table",
                    },
                    .index_expression = &Expression{
                        .StringLiteral = "foo",
                    },
                },
            },
        },
    };

    try runTests(ExpressionTestCase, "parse index expression", &test_cases, runExpressionTest);
}

test "parse program" {
    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: ExpressionTestCase) anyerror!void {
            const program = try getProgram(arena, test_case.input);
            return expectExpression(test_case.expected_expression, program);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "fibonacci",
            .input =
            \\fib = (n) -> {
            \\    a = 0
            \\    b = 1
            \\    for _ in 0..n {
            \\        tmp = b
            \\        b = a + b
            \\        a = tmp
            \\    }
            \\    a
            \\}
            \\
            \\fib(30)
            ,
            .expected_expression = Expression{
                .Program = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .Statement = &Expression{
                            .AssignmentExpression = AssignmentExpression{
                                .left = &Expression{ .Identifier = "fib" },
                                .expression = &Expression{
                                    .FunctionLiteral = FunctionLiteral{
                                        .params = try list(FunctionParam, arena.allocator(), &.{
                                            FunctionParam{ .Identifier = "n" },
                                        }),
                                        .body = &Expression{
                                            .BlockExpression = BlockExpression{
                                                .expressions = try list(*const Expression, arena.allocator(), &.{
                                                    &Expression{
                                                        .Statement = &Expression{
                                                            .AssignmentExpression = AssignmentExpression{
                                                                .left = &Expression{
                                                                    .Identifier = "a",
                                                                },
                                                                .expression = &Expression{
                                                                    .IntegerLiteral = 0,
                                                                },
                                                            },
                                                        },
                                                    },
                                                    &Expression{
                                                        .Statement = &Expression{
                                                            .AssignmentExpression = AssignmentExpression{
                                                                .left = &Expression{
                                                                    .Identifier = "b",
                                                                },
                                                                .expression = &Expression{
                                                                    .IntegerLiteral = 1,
                                                                },
                                                            },
                                                        },
                                                    },
                                                    &Expression{
                                                        .Statement = &Expression{
                                                            .ForExpression = ForExpression{
                                                                .variable = "_",
                                                                .range = RangeExpression{
                                                                    .left = &Expression{
                                                                        .IntegerLiteral = 0,
                                                                    },
                                                                    .right = &Expression{
                                                                        .Identifier = "n",
                                                                    },
                                                                },
                                                                .body = BlockExpression{
                                                                    .expressions = try list(*const Expression, arena.allocator(), &.{
                                                                        &Expression{
                                                                            .Statement = &Expression{
                                                                                .AssignmentExpression = AssignmentExpression{
                                                                                    .left = &Expression{
                                                                                        .Identifier = "tmp",
                                                                                    },
                                                                                    .expression = &Expression{
                                                                                        .Identifier = "b",
                                                                                    },
                                                                                },
                                                                            },
                                                                        },
                                                                        &Expression{
                                                                            .Statement = &Expression{
                                                                                .AssignmentExpression = AssignmentExpression{
                                                                                    .left = &Expression{
                                                                                        .Identifier = "b",
                                                                                    },
                                                                                    .expression = &Expression{
                                                                                        .InfixExpression = InfixExpression{
                                                                                            .operator = .Plus,
                                                                                            .left = &Expression{
                                                                                                .Identifier = "a",
                                                                                            },
                                                                                            .right = &Expression{
                                                                                                .Identifier = "b",
                                                                                            },
                                                                                        },
                                                                                    },
                                                                                },
                                                                            },
                                                                        },
                                                                        &Expression{
                                                                            .Statement = &Expression{
                                                                                .AssignmentExpression = AssignmentExpression{
                                                                                    .left = &Expression{
                                                                                        .Identifier = "a",
                                                                                    },
                                                                                    .expression = &Expression{
                                                                                        .Identifier = "tmp",
                                                                                    },
                                                                                },
                                                                            },
                                                                        },
                                                                    }),
                                                                },
                                                            },
                                                        },
                                                    },
                                                    &Expression{
                                                        .Statement = &Expression{
                                                            .Identifier = "a",
                                                        },
                                                    },
                                                }),
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    &Expression{
                        .Statement = &Expression{
                            .CallExpression = CallExpression{
                                .function = &Expression{
                                    .Identifier = "fib",
                                },
                                .args = try list(*const Expression, arena.allocator(), &.{
                                    &Expression{
                                        .IntegerLiteral = 30,
                                    },
                                }),
                            },
                        },
                    },
                }),
            },
        },
    };

    try runTests(ExpressionTestCase, "parse program", &test_cases, run);
}
