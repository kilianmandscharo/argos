const std = @import("std");
const expect = std.testing.expect;

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;
const Expression = parser_module.Expression;
const PrefixExpression = parser_module.PrefixExpression;
const InfixExpression = parser_module.InfixExpression;
const FunctionLiteral = parser_module.FunctionLiteral;
const Statement = parser_module.Statement;
const BlockStatement = parser_module.BlockStatement;
const Operator = parser_module.Operator;
const ParserError = parser_module.ParserError;

fn getProgram(arena: std.mem.Allocator, content: []const u8) !std.ArrayList(Statement) {
    var lexer = try Lexer.init(arena, content);
    var parser = try Parser.init(&lexer, arena);

    const program = try parser.parseProgram();
    return program;
}

fn expectFormattedProgram(arena: std.mem.Allocator, input: []const u8, expected_output: []const u8) !void {
    var lexer = try Lexer.init(arena, input);
    var parser = try Parser.init(&lexer, arena);

    const program = try parser.parseProgram();
    const parsed = try Parser.printProgram(program, arena);

    try std.testing.expectEqualStrings(expected_output, parsed);
}

fn getExpression(arena: std.mem.Allocator, content: []const u8) !*const Expression {
    var lexer = try Lexer.init(arena, content);
    var parser = try Parser.init(&lexer, arena);

    const program = try parser.parseProgram();

    try expect(program.items.len == 1);
    const statement = program.items[0];
    try expect(statement == .ExpressionStatement);
    const expression_statement = statement.ExpressionStatement;

    return expression_statement.expression;
}

fn expectStatement(expected: Statement, actual: Statement) anyerror!void {
    const Tag = std.meta.Tag(@TypeOf(expected));

    const expectedTag = @as(Tag, expected);
    const actualTag = @as(Tag, actual);

    try std.testing.expectEqual(expectedTag, actualTag);

    return switch (expected) {
        .AssignmentStatement => |assignment_stmt| {
            try std.testing.expectEqualStrings(assignment_stmt.identifier.literal, actual.AssignmentStatement.identifier.literal);
            try expectExpression(assignment_stmt.expression.*, actual.AssignmentStatement.expression.*);
        },
        .ReturnStatement => |return_stmt| {
            try expectExpression(return_stmt.expression.*, actual.ReturnStatement.expression.*);
        },
        .ExpressionStatement => |expression_stmt| {
            try expectExpression(expression_stmt.expression.*, actual.ExpressionStatement.expression.*);
        },
        .BlockStatement => |block_stmt| {
            try std.testing.expectEqual(block_stmt.statements.items.len, actual.BlockStatement.statements.items.len);
            try std.testing.expectEqual(block_stmt.is_one_liner, actual.BlockStatement.is_one_liner);
            for (block_stmt.statements.items, 0..) |stmt, i| {
                try expectStatement(stmt, actual.BlockStatement.statements.items[i]);
            }
        },
    };
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
            try expectStatement(Statement{ .BlockStatement = func_literal.body }, Statement{ .BlockStatement = actual.FunctionLiteral.body });
        },
        else => try std.testing.expectEqual(expected, actual),
    };
}

fn runTests(comptime T: type, name: []const u8, test_cases: []const T, run: fn (arena: std.mem.Allocator, test_case: T) anyerror!void) void {
    std.debug.print("--- start {s} tests ---\n", .{name});

    for (test_cases) |test_case| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const result = run(arena.allocator(), test_case);

        if (result) |_| {
            std.debug.print("🟢 {s}\n", .{test_case.description});
        } else |_| {
            std.debug.print("🔴 {s}\n", .{test_case.description});
        }
    }

    std.debug.print("--- end {s} tests ---\n\n", .{name});
}

test "parse expressions" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_output: Expression,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            const expression = try getExpression(arena, test_case.input);
            return expectExpression(test_case.expected_output, expression.*);
        }
    }.runTest;

    const test_cases = [_]TestCase{
        .{
            .description = "should parse identifier",
            .input =
            \\foo
            ,
            .expected_output = Expression{ .Identifier = "foo" },
        },
        .{
            .description = "should parse string literal",
            .input =
            \\"foo"
            ,
            .expected_output = Expression{ .StringLiteral = "foo" },
        },
        .{
            .description = "should parse integer literal",
            .input =
            \\666
            ,
            .expected_output = Expression{ .IntegerLiteral = 666 },
        },
        .{
            .description = "should parse float literal",
            .input =
            \\3.1415
            ,
            .expected_output = Expression{ .FloatLiteral = 3.1415 },
        },
        .{
            .description = "should parse true",
            .input =
            \\true
            ,
            .expected_output = Expression{ .BooleanLiteral = true },
        },
        .{
            .description = "should parse false",
            .input =
            \\false
            ,
            .expected_output = Expression{ .BooleanLiteral = false },
        },
    };

    runTests(TestCase, "parse expressions", &test_cases, run);
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

    runTests(TestCase, "parse prefix expression", &test_cases, run);
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

    runTests(TestCase, "parse infix expression", &test_cases, run);
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
                    .body = BlockStatement{
                        .is_one_liner = true,
                        .statements = .{},
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
                    .body = BlockStatement{
                        .is_one_liner = false,
                        .statements = .{},
                    },
                },
            },
            .expected_error = null,
        },
        // .{
        //     .description = "one liner with params",
        //     .input =
        //     \\fnc test(a, b) { return a + b }
        //     ,
        //     .expected_expression = Expression{
        //         .FunctionLiteral = FunctionLiteral{
        //             .name = "test",
        //             .is_one_liner = false,
        //             .params = .{
        //                 Expression{ .Identifier = "a" },
        //                 Expression{ .Identifier = "b" },
        //             },
        //             .body = BlockStatement{
        //                 .is_one_liner = false,
        //                 .statements = .{},
        //             },
        //         },
        //     },
        //     .expected_error = null,
        // },
        // .{
        //     .description = "multi liner with params and one statement",
        //     .input =
        //     \\fnc test(a, b) {
        //     \\    return a + b
        //     \\}
        //     ,
        //     .expected =
        //     \\fnc test(a, b) {
        //     \\    return (a + b)
        //     \\}
        //     ,
        //     .expected_error = null,
        // },
        // .{
        //     .description = "multi liner with params and multiple statements",
        //     .input =
        //     \\fnc test(a, b) {
        //     \\    x = a * b
        //     \\    y = a / b
        //     \\    return x + y
        //     \\}
        //     ,
        //     .expected =
        //     \\fnc test(a, b) {
        //     \\    x = (a * b)
        //     \\    y = (a / b)
        //     \\    return (x + y)
        //     \\}
        //     ,
        //     .expected_error = null,
        // },
        // .{
        //     .description = "multi liner with gaps",
        //     .input =
        //     \\fnc test(a, b) {
        //     \\    x = a * b
        //     \\    y = a / b
        //     \\    return x + y
        //     \\}
        //     ,
        //     .expected =
        //     \\fnc test(a, b) {
        //     \\    x = (a * b)
        //     \\    y = (a / b)
        //     \\    return (x + y)
        //     \\}
        //     ,
        //     .expected_error = null,
        // },
        // .{
        //     .description = "one liner without return",
        //     .input =
        //     \\fnc test(a, b) { a - b }
        //     ,
        //     .expected =
        //     \\fnc test(a, b) { (a - b) }
        //     ,
        //     .expected_error = null,
        // },
        // .{
        //     .description = "multi liner without return",
        //     .input =
        //     \\fnc test(a, b) {
        //     \\    x = a * b
        //     \\    y = a / b
        //     \\    x + y
        //     \\}
        //     ,
        //     .expected =
        //     \\fnc test(a, b) {
        //     \\    x = (a * b)
        //     \\    y = (a / b)
        //     \\    (x + y)
        //     \\}
        //     ,
        //     .expected_error = null,
        // },
        // .{
        //     .description = "new line before closing brace is needed for multi liner",
        //     .input =
        //     \\fnc test(a, b) {
        //     \\    x = a * b
        //     \\    y = a / b
        //     \\    x + y }
        //     ,
        //     .expected = "",
        //     .expected_error = ParserError.UnexpectedTokenType,
        // },
        // .{
        //     .description = "new line after opening brace is needed for multi liner",
        //     .input =
        //     \\fnc test(a, b) { x = a * b
        //     \\    y = a / b
        //     \\    x + y
        //     \\}
        //     ,
        //     .expected = "",
        //     .expected_error = ParserError.UnexpectedTokenType,
        // },
        // .{
        //     .description = "multi liner with assignment on new line",
        //     .input =
        //     \\fnc test(a, b) {
        //     \\    x = a * b
        //     \\    y = a / b
        //     \\    x + y
        //     \\}
        //     \\
        //     \\result = test(a, b)
        //     ,
        //     .expected =
        //     \\fnc test(a, b) {
        //     \\    x = (a * b)
        //     \\    y = (a / b)
        //     \\    (x + y)
        //     \\}
        //     \\result = (test(a, b))
        //     ,
        //     .expected_error = null,
        // },
        // .{
        //     .description = "multi liner with expression on new line",
        //     .input =
        //     \\fnc test(a, b) {
        //     \\    x = a * b
        //     \\    y = a / b
        //     \\    x + y
        //     \\}
        //     \\
        //     \\test(a, b)
        //     ,
        //     .expected =
        //     \\fnc test(a, b) {
        //     \\    x = (a * b)
        //     \\    y = (a / b)
        //     \\    (x + y)
        //     \\}
        //     \\(test(a, b))
        //     ,
        //     .expected_error = null,
        // },
    };

    runTests(TestCase, "parse function declaration", &test_cases, run);
}

test "should parse function call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\val = fnc test(a, b) {
        \\    return a + b
        \\}(1, 2)
    ;

    const expected_output =
        \\val = (fnc test(a, b) {
        \\    return (a + b)
        \\}(1, 2))
    ;

    try expectFormattedProgram(arena.allocator(), content, expected_output);
}

test "should parse empty one liner function call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\val = fnc test(a, b) { return a + b }(1, 2)
    ;

    const expected_output =
        \\val = (fnc test(a, b) { return (a + b) }(1, 2))
    ;

    try expectFormattedProgram(arena.allocator(), content, expected_output);
}

test "should parse function declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\fnc test(a, b) {
        \\    return a + b
        \\}
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expect(expression.* == .FunctionLiteral);
    const function_literal = expression.FunctionLiteral;
    try expect(std.mem.eql(u8, function_literal.name, "test"));
    try expect(function_literal.params.items.len == 2);
    try expect(std.mem.eql(u8, function_literal.params.items[0], "a"));
    try expect(std.mem.eql(u8, function_literal.params.items[1], "b"));
    try expect(function_literal.body.statements.items.len == 1);
    const return_statement = function_literal.body.statements.items[0];
    try expect(return_statement == .ReturnStatement);
    try expect(return_statement.ReturnStatement.expression.* == .InfixExpression);
    const infix_expression = return_statement.ReturnStatement.expression.*.InfixExpression;
    try expect(infix_expression.operator == .Plus);
    try expect(infix_expression.left.* == .Identifier);
    try expect(infix_expression.right.* == .Identifier);
    try expect(std.mem.eql(u8, infix_expression.left.*.Identifier, "a"));
    try expect(std.mem.eql(u8, infix_expression.right.*.Identifier, "b"));
}

test "should parse function declaration in one line" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\fnc test(a, b) { return a + b }
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expect(expression.* == .FunctionLiteral);
    const function_literal = expression.FunctionLiteral;
    try expect(std.mem.eql(u8, function_literal.name, "test"));
    try expect(function_literal.params.items.len == 2);
    try expect(std.mem.eql(u8, function_literal.params.items[0], "a"));
    try expect(std.mem.eql(u8, function_literal.params.items[1], "b"));
    try expect(function_literal.body.statements.items.len == 1);
    const return_statement = function_literal.body.statements.items[0];
    try expect(return_statement == .ReturnStatement);
    try expect(return_statement.ReturnStatement.expression.* == .InfixExpression);
    const infix_expression = return_statement.ReturnStatement.expression.*.InfixExpression;
    try expect(infix_expression.operator == .Plus);
    try expect(infix_expression.left.* == .Identifier);
    try expect(infix_expression.right.* == .Identifier);
    try expect(std.mem.eql(u8, infix_expression.left.*.Identifier, "a"));
    try expect(std.mem.eql(u8, infix_expression.right.*.Identifier, "b"));
}

test "if expressions" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected: []const u8,
        expected_error: ?ParserError,
    };

    const test_cases = [_]TestCase{
        .{
            .description = "single line empty no else",
            .input =
            \\if 5 == 5 {}
            ,
            .expected =
            \\if (5 == 5) {}
            ,
            .expected_error = null,
        },
        .{
            .description = "single line empty with else",
            .input =
            \\if 5 == 5 {} else {}
            ,
            .expected =
            \\if (5 == 5) {} else {}
            ,
            .expected_error = null,
        },
        .{
            .description = "single line no else",
            .input =
            \\if 5 == 5 { 3 + 2 }
            ,
            .expected =
            \\if (5 == 5) { (3 + 2) }
            ,
            .expected_error = null,
        },
        .{
            .description = "multi line empty no else with empty line",
            .input =
            \\if 5 == 5 {
            \\
            \\}
            ,
            .expected =
            \\if (5 == 5) {}
            ,
            .expected_error = null,
        },
        .{
            .description = "multi line empty no else",
            .input =
            \\if 5 == 5 {
            \\}
            ,
            .expected =
            \\if (5 == 5) {}
            ,
            .expected_error = null,
        },
        .{
            .description = "multi line empty with else",
            .input =
            \\if 5 == 5 {
            \\} else {
            \\}
            ,
            .expected =
            \\if (5 == 5) {} else {}
            ,
            .expected_error = null,
        },
        .{
            .description = "multi line no else",
            .input =
            \\if 5 == 5 {
            \\    3 + 2
            \\}
            ,
            .expected =
            \\if (5 == 5) {
            \\    (3 + 2)
            \\}
            ,
            .expected_error = null,
        },
        .{
            .description = "single line with else",
            .input =
            \\if 5 == 5 { 3 + 2 } else { 1 * 1 }
            ,
            .expected =
            \\if (5 == 5) { (3 + 2) } else { (1 * 1) }
            ,
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
            .expected =
            \\if (5 == 5) {
            \\    (3 + 2)
            \\} else {
            \\    (1 * 1)
            \\}
            ,
            .expected_error = null,
        },
    };

    std.debug.print("--- start if expressions tests ---\n", .{});
    for (test_cases) |test_case| {
        std.debug.print("  --> {s}\n", .{test_case.description});
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const result = expectFormattedProgram(arena.allocator(), test_case.input, test_case.expected);
        if (test_case.expected_error == null) {
            try result;
        } else {
            try std.testing.expectError(ParserError.UnexpectedTokenType, result);
        }
    }
    std.debug.print("--- end if expressions tests ---\n", .{});
}

test "range expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected: []const u8,
        expected_error: ?ParserError,
    };

    const test_cases = [_]TestCase{
        .{
            .description = "two integers",
            .input =
            \\0..10
            ,
            .expected =
            \\(0..10)
            ,
            .expected_error = null,
        },
        .{
            .description = "two infix expressions",
            .input =
            \\2 + 3..50 - 10
            ,
            .expected =
            \\((2 + 3)..(50 - 10))
            ,
            .expected_error = null,
        },
        .{
            .description = "two function calls",
            .input =
            \\start()..end()
            ,
            .expected =
            \\((start())..(end()))
            ,
            .expected_error = null,
        },
    };

    std.debug.print("--- start range expressions tests ---\n", .{});
    for (test_cases) |test_case| {
        std.debug.print("  --> {s}\n", .{test_case.description});
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const result = expectFormattedProgram(arena.allocator(), test_case.input, test_case.expected);
        if (test_case.expected_error == null) {
            try result;
        } else {
            try std.testing.expectError(ParserError.UnexpectedTokenType, result);
        }
    }
    std.debug.print("--- end range expressions tests ---\n", .{});
}

test "for expression" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected: []const u8,
        expected_error: ?ParserError,
    };

    const test_cases = [_]TestCase{
        .{
            .description = "simple for loop",
            .input =
            \\for i in 0..10 {
            \\    x = x * i
            \\}
            ,
            .expected =
            \\for i in (0..10) {
            \\    x = (x * i)
            \\}
            ,
            .expected_error = null,
        },
    };

    std.debug.print("--- start for expressions tests ---\n", .{});
    for (test_cases) |test_case| {
        std.debug.print("  --> {s}\n", .{test_case.description});
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const result = expectFormattedProgram(arena.allocator(), test_case.input, test_case.expected);
        if (test_case.expected_error == null) {
            try result;
        } else {
            try std.testing.expectError(ParserError.UnexpectedTokenType, result);
        }
    }
    std.debug.print("--- end for expressions tests ---\n", .{});
}
