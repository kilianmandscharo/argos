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
const BlockExpression = parser_module.BlockExpression;
const IndexExpression = parser_module.IndexExpression;
const AssignmentExpression = parser_module.AssignmentExpression;
const Operator = parser_module.Operator;
const ParserError = parser_module.ParserError;

const test_utils = @import("test_utils.zig");
const runTests = test_utils.runTests;
const list = test_utils.list;

// TODO: add error cases

fn getProgram(arena: std.mem.Allocator, content: []const u8) !std.ArrayListUnmanaged(*const Expression) {
    var lexer = try Lexer.init(arena, content);
    var parser = try Parser.init(&lexer, arena);
    return try parser.parseProgram();
}

fn getExpression(arena: std.mem.Allocator, content: []const u8) !*const Expression {
    const program = try getProgram(arena, content);
    try expect(program.items.len >= 1);
    return program.items[0];
}

fn expectExpression(expected: Expression, actual: Expression) !void {
    const Tag = std.meta.Tag(@TypeOf(expected));

    const expectedTag = @as(Tag, expected);
    const actualTag = @as(Tag, actual);

    try std.testing.expectEqual(expectedTag, actualTag);

    return switch (expected) {
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
            try std.testing.expectEqualStrings(func_literal.name, actual.FunctionLiteral.name);
            try std.testing.expectEqual(func_literal.is_one_liner, actual.FunctionLiteral.is_one_liner);
            try std.testing.expectEqual(func_literal.params.items.len, actual.FunctionLiteral.params.items.len);
            try expectExpression(Expression{ .BlockExpression = func_literal.body }, Expression{ .BlockExpression = actual.FunctionLiteral.body });
        },
        .IfExpression => |if_expression| {
            try expectExpression(if_expression.condition.*, actual.IfExpression.condition.*);
            try std.testing.expectEqual(if_expression.is_one_liner, actual.IfExpression.is_one_liner);
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
            try std.testing.expectEqual(block_expression.is_one_liner, actual.BlockExpression.is_one_liner);
            for (block_expression.expressions.items, 0..) |expression, i| {
                try expectExpression(expression.*, actual.BlockExpression.expressions.items[i].*);
            }
        },
        .AssignmentExpression => |assignment_expression| {
            try std.testing.expectEqualStrings(assignment_expression.identifier, actual.AssignmentExpression.identifier);
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
    };
}

test "parse expressions" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    const test_cases = [_]TestCase{
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

    try runTests(TestCase, "parse expressions", &test_cases, run);
}

test "should parse prefix expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    const test_cases = [_]TestCase{
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

    try runTests(TestCase, "parse prefix expression", &test_cases, run);
}

test "should parse infix expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    const test_cases = [_]TestCase{
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
    };

    try runTests(TestCase, "parse infix expression", &test_cases, run);
}

test "function declaration" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
        .{
            .description = "empty function one line",
            .input =
            \\fnc test() {}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .name = "test",
                    .is_one_liner = true,
                    .params = .{},
                    .body = BlockExpression{
                        .is_one_liner = true,
                        .expressions = .{},
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "empty function multi lines",
            .input =
            \\fnc test() {
            \\
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .name = "test",
                    .is_one_liner = false,
                    .params = .{},
                    .body = BlockExpression{
                        .is_one_liner = false,
                        .expressions = .{},
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "one liner with params",
            .input =
            \\fnc test(a, b) { return a + b }
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .name = "test",
                    .is_one_liner = true,
                    .params = try list([]const u8, arena.allocator(), &.{
                        "a",
                        "b",
                    }),
                    .body = BlockExpression{
                        .is_one_liner = true,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "multiple lines with params",
            .input =
            \\fnc test(a, b) {
            \\    return a + b
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .name = "test",
                    .is_one_liner = false,
                    .params = try list([]const u8, arena.allocator(), &.{
                        "a",
                        "b",
                    }),
                    .body = BlockExpression{
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "multi liner with params and multiple statements",
            .input =
            \\fnc test(a, b) {
            \\    x = a * b
            \\    y = a / b
            \\    return x + y
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .name = "test",
                    .is_one_liner = false,
                    .params = try list([]const u8, arena.allocator(), &.{
                        "a",
                        "b",
                    }),
                    .body = BlockExpression{
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "x",
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
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "y",
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
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "multi liner with gaps",
            .input =
            \\fnc test(a, b) {
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
                    .name = "test",
                    .is_one_liner = false,
                    .params = try list([]const u8, arena.allocator(), &.{
                        "a",
                        "b",
                    }),
                    .body = BlockExpression{
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "x",
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
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "y",
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
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "one liner without return",
            .input =
            \\fnc test(a, b) { a + b }
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .name = "test",
                    .is_one_liner = true,
                    .params = try list([]const u8, arena.allocator(), &.{
                        "a",
                        "b",
                    }),
                    .body = BlockExpression{
                        .is_one_liner = true,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "multi liner without return",
            .input =
            \\fnc test(a, b) {
            \\    x = a * b
            \\    y = a / b
            \\    x + y
            \\}
            ,
            .expected_expression = Expression{
                .FunctionLiteral = FunctionLiteral{
                    .name = "test",
                    .is_one_liner = false,
                    .params = try list([]const u8, arena.allocator(), &.{
                        "a",
                        "b",
                    }),
                    .body = BlockExpression{
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "x",
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
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "y",
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
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse function declaration", &test_cases, run);
}

test "if expressions" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
        .{
            .description = "single line empty no else",
            .input =
            \\if 5 == 5 {}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .is_one_liner = true,
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
                        .is_one_liner = true,
                        .expressions = .{},
                    },
                    .alternative = null,
                },
            },
            .expected_error = null,
        },
        .{
            .description = "single line empty with else",
            .input =
            \\if 5 == 5 {} else {}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .is_one_liner = true,
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
                        .is_one_liner = true,
                        .expressions = .{},
                    },
                    .alternative = BlockExpression{
                        .is_one_liner = true,
                        .expressions = .{},
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "single line no else",
            .input =
            \\if 5 == 5 { 3 + 2 }
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .is_one_liner = true,
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
                        .is_one_liner = true,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                    .alternative = null,
                },
            },
            .expected_error = null,
        },
        .{
            .description = "single line with else",
            .input =
            \\if 5 == 5 { 3 + 2 } else { 1 * 1 }
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .is_one_liner = true,
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
                        .is_one_liner = true,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                    .alternative = BlockExpression{
                        .is_one_liner = true,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
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
                    .is_one_liner = false,
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
                        .is_one_liner = false,
                        .expressions = .{},
                    },
                    .alternative = null,
                },
            },
            .expected_error = null,
        },
        .{
            .description = "multi line empty no else",
            .input =
            \\if 5 == 5 {
            \\}
            ,
            .expected_expression = Expression{
                .IfExpression = IfExpression{
                    .is_one_liner = false,
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
                        .is_one_liner = false,
                        .expressions = .{},
                    },
                    .alternative = null,
                },
            },
            .expected_error = null,
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
                    .is_one_liner = false,
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
                        .is_one_liner = false,
                        .expressions = .{},
                    },
                    .alternative = BlockExpression{
                        .is_one_liner = false,
                        .expressions = .{},
                    },
                },
            },
            .expected_error = null,
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
                    .is_one_liner = false,
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
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                    .alternative = null,
                },
            },
            .expected_error = null,
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
                    .is_one_liner = false,
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
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                    .alternative = BlockExpression{
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse if expression", &test_cases, run);
}

test "range expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    const test_cases = [_]TestCase{
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
            .expected_error = null,
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
            .expected_error = null,
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
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse range expression", &test_cases, run);
}

test "for expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
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
                        .is_one_liner = false,
                        .expressions = try list(*const Expression, arena.allocator(), &.{
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "x",
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
                        }),
                    },
                },
            },
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse for expression", &test_cases, run);
}

test "function call" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
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
            .expected_error = null,
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
            .expected_error = null,
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
            .expected_error = null,
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
            .expected_error = null,
        },
        .{
            .description = "iife",
            .input =
            \\fnc test(a, b) {
            \\    return a + b
            \\}(1, 2)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .FunctionLiteral = FunctionLiteral{
                            .is_one_liner = false,
                            .name = "test",
                            .params = try list([]const u8, arena.allocator(), &.{
                                "a",
                                "b",
                            }),
                            .body = BlockExpression{
                                .is_one_liner = false,
                                .expressions = try list(*const Expression, arena.allocator(), &.{
                                    &Expression{
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
                                }),
                            },
                        },
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
            .expected_error = null,
        },
        .{
            .description = "one liner iife",
            .input =
            \\fnc test(a, b) { return a + b }(1, 2)
            ,
            .expected_expression = Expression{
                .CallExpression = CallExpression{
                    .function = &Expression{
                        .FunctionLiteral = FunctionLiteral{
                            .is_one_liner = true,
                            .name = "test",
                            .params = try list([]const u8, arena.allocator(), &.{
                                "a",
                                "b",
                            }),
                            .body = BlockExpression{
                                .is_one_liner = true,
                                .expressions = try list(*const Expression, arena.allocator(), &.{
                                    &Expression{
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
                                }),
                            },
                        },
                    },
                    .args = try list(*const Expression, arena.allocator(), &.{
                        &Expression{ .IntegerLiteral = 1 },
                        &Expression{ .IntegerLiteral = 2 },
                    }),
                },
            },
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse function call", &test_cases, run);
}

test "array literal" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
        .{
            .description = "integer array one line no trailing comma no element",
            .input =
            \\[]
            ,
            .expected_expression = Expression{
                .ArrayLiteral = try list(*const Expression, arena.allocator(), &.{}),
            },
            .expected_error = null,
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
            .expected_error = null,
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
            .expected_error = null,
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
            .expected_error = null,
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
            .expected_error = null,
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
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse array literal", &test_cases, run);
}

test "parse program" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_output: std.ArrayListUnmanaged(*const Expression),
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const program = try getProgram(arena, test_case.input);
            try std.testing.expectEqual(test_case.expected_output.items.len, program.items.len);
            for (test_case.expected_output.items, 0..) |expression, i| {
                try expectExpression(expression.*, program.items[i].*);
            }
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
        .{
            .description = "fibonacci",
            .input =
            \\fnc fib(n) {
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
            .expected_output = try list(*const Expression, arena.allocator(), &.{
                &Expression{
                    .FunctionLiteral = FunctionLiteral{
                        .is_one_liner = false,
                        .name = "fib",
                        .params = try list([]const u8, arena.allocator(), &.{
                            "n",
                        }),
                        .body = BlockExpression{
                            .is_one_liner = false,
                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                &Expression{
                                    .AssignmentExpression = AssignmentExpression{
                                        .identifier = "a",
                                        .expression = &Expression{
                                            .IntegerLiteral = 0,
                                        },
                                    },
                                },
                                &Expression{
                                    .AssignmentExpression = AssignmentExpression{
                                        .identifier = "b",
                                        .expression = &Expression{
                                            .IntegerLiteral = 1,
                                        },
                                    },
                                },
                                &Expression{
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
                                            .is_one_liner = false,
                                            .expressions = try list(*const Expression, arena.allocator(), &.{
                                                &Expression{
                                                    .AssignmentExpression = AssignmentExpression{
                                                        .identifier = "tmp",
                                                        .expression = &Expression{
                                                            .Identifier = "b",
                                                        },
                                                    },
                                                },
                                                &Expression{
                                                    .AssignmentExpression = AssignmentExpression{
                                                        .identifier = "b",
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
                                                &Expression{
                                                    .AssignmentExpression = AssignmentExpression{
                                                        .identifier = "a",
                                                        .expression = &Expression{
                                                            .Identifier = "tmp",
                                                        },
                                                    },
                                                },
                                            }),
                                        },
                                    },
                                },
                                &Expression{
                                    .Identifier = "a",
                                },
                            }),
                        },
                    },
                },
                &Expression{
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
            }),
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse statement", &test_cases, run);
}

test "block expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
        .{
            .description = "block expression one line",
            .input =
            \\{ x = 1 }
            ,
            .expected_expression = Expression{
                .BlockExpression = BlockExpression{
                    .is_one_liner = true,
                    .expressions = try list(*const Expression, arena.allocator(), &.{
                        &Expression{
                            .AssignmentExpression = AssignmentExpression{
                                .identifier = "x",
                                .expression = &Expression{
                                    .IntegerLiteral = 1,
                                },
                            },
                        },
                    }),
                },
            },
            .expected_error = null,
        },
        .{
            .description = "block expression multi line",
            .input =
            \\{
            \\     x = 1
            \\     y = 2
            \\     z = x * y
            \\}
            ,
            .expected_expression = Expression{
                .BlockExpression = BlockExpression{
                    .is_one_liner = false,
                    .expressions = try list(*const Expression, arena.allocator(), &.{
                        &Expression{
                            .AssignmentExpression = AssignmentExpression{
                                .identifier = "x",
                                .expression = &Expression{
                                    .IntegerLiteral = 1,
                                },
                            },
                        },
                        &Expression{
                            .AssignmentExpression = AssignmentExpression{
                                .identifier = "y",
                                .expression = &Expression{
                                    .IntegerLiteral = 2,
                                },
                            },
                        },
                        &Expression{
                            .AssignmentExpression = AssignmentExpression{
                                .identifier = "z",
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
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse block expression", &test_cases, run);
}

test "assignment expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    const test_cases = [_]TestCase{
        .{
            .description = "assignment expression",
            .input =
            \\x = 5
            ,
            .expected_expression = Expression{
                .AssignmentExpression = AssignmentExpression{
                    .identifier = "x",
                    .expression = &Expression{
                        .IntegerLiteral = 5,
                    },
                },
            },
            .expected_error = null,
        },
        .{
            .description = "nested assignment expression",
            .input =
            \\x = y = 5
            ,
            .expected_expression = Expression{
                .AssignmentExpression = AssignmentExpression{
                    .identifier = "x",
                    .expression = &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .identifier = "y",
                            .expression = &Expression{
                                .IntegerLiteral = 5,
                            },
                        },
                    },
                },
            },
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse assignment expression", &test_cases, run);
}

test "table expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
        .{
            .description = "table expression one line",
            .input =
            \\{ x = 1, }
            ,
            .expected_expression = Expression{
                .TableLiteral = try list(*const Expression, arena.allocator(), &.{
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .identifier = "x",
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                }),
            },
            .expected_error = null,
        },
        .{
            .description = "table expression multi line",
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
                            .identifier = "x",
                            .expression = &Expression{
                                .IntegerLiteral = 1,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .identifier = "y",
                            .expression = &Expression{
                                .IntegerLiteral = 2,
                            },
                        },
                    },
                    &Expression{
                        .AssignmentExpression = AssignmentExpression{
                            .identifier = "z",
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
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse table expression", &test_cases, run);
}

test "index expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_expression: Expression,
        expected_error: ?ParserError,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_expression, expression.*);
        }
    }.runTest;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]TestCase{
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
            .expected_error = null,
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
            .expected_error = null,
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
                                    .identifier = "a",
                                    .expression = &Expression{
                                        .IntegerLiteral = 6,
                                    },
                                },
                            },
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "b",
                                    .expression = &Expression{
                                        .IntegerLiteral = 6,
                                    },
                                },
                            },
                            &Expression{
                                .AssignmentExpression = AssignmentExpression{
                                    .identifier = "c",
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
            .expected_error = null,
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
            .expected_error = null,
        },
    };

    try runTests(TestCase, "parse index expression", &test_cases, run);
}
