const std = @import("std");
const expect = std.testing.expect;

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;
const Expression = parser_module.Expression;
const Statement = parser_module.Statement;
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

fn expectIdentifier(expression: *const Expression, name: []const u8) !void {
    try expect(expression.* == .Identifier);
    try expect(std.mem.eql(u8, expression.*.Identifier, name));
}

fn expectStringLiteral(expression: *const Expression, value: []const u8) !void {
    try expect(expression.* == .StringLiteral);
    try expect(std.mem.eql(u8, expression.*.StringLiteral, value));
}

fn expectIntegerLiteral(expression: *const Expression, value: i64) !void {
    try expect(expression.* == .IntegerLiteral);
    try expect(expression.*.IntegerLiteral == value);
}

fn expectFloatLiteral(expression: *const Expression, value: f64) !void {
    try expect(expression.* == .FloatLiteral);
    try expect(expression.*.FloatLiteral == value);
}

fn expectBooleanLiteral(expression: *const Expression, value: bool) !void {
    try expect(expression.* == .BooleanLiteral);
    try expect(expression.*.BooleanLiteral == value);
}

fn expectStatement(want: Statement, got: Statement) !void {
    try expect(@tagName(want) == @tagName(got));

    switch (want) {
        .AssignmentStatement => |v| {
            expect(std.mem.eql(u8, v.identifier.literal, got.AssignmentStatement.identifier.literal));
            expect(std.meta.eql(v.expression.*, got.AssignmentStatement.expression.*));
        },
        .ReturnStatement => |v| {
            expect(std.meta.eql(v.expression.*, got.ReturnStatement.expression.*));
        },
        .ExpressionStatement => |v| {
            expect(std.meta.eql(v.expression.*, got.ExpressionStatement.expression.*));
        },
        .BlockStatement => |v| {
            for (0..v.statements.items.len) |i| {
                try expectStatement(v.statements.items[i], got.BlockStatement.statements.items[i]);
            }
        },
    }
}

test "should parse identifier" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\foo
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expectIdentifier(expression, "foo");
}

test "should parse string literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\"foo"
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expectStringLiteral(expression, "foo");
}

test "should parse integer literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\42
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expectIntegerLiteral(expression, 42);
}

test "should parse float literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\3.1415
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expectFloatLiteral(expression, 3.1415);
}

test "should parse true" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\true
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expectBooleanLiteral(expression, true);
}

test "should parse false" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\false
    ;

    const expression = try getExpression(arena.allocator(), content);
    try expectBooleanLiteral(expression, false);
}

test "should parse prefix expression" {
    const TestCase = struct {
        content: []const u8,
        operator: Operator,
        expression: Expression,
    };

    const test_cases = [_]TestCase{
        .{ .content = "!true", .operator = .Bang, .expression = Expression{ .BooleanLiteral = true } },
        .{ .content = "!false", .operator = .Bang, .expression = Expression{ .BooleanLiteral = false } },
        .{ .content = "+5", .operator = .Plus, .expression = Expression{ .IntegerLiteral = 5 } },
        .{ .content = "-2", .operator = .Minus, .expression = Expression{ .IntegerLiteral = 2 } },
        .{ .content = "+4.15", .operator = .Plus, .expression = Expression{ .FloatLiteral = 4.15 } },
        .{ .content = "-0.668", .operator = .Minus, .expression = Expression{ .FloatLiteral = 0.668 } },
    };

    for (test_cases) |test_case| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const expression = try getExpression(arena.allocator(), test_case.content);
        try expect(expression.* == .PrefixExpression);
        try expect(expression.*.PrefixExpression.operator == test_case.operator);
        try expect(std.meta.eql(expression.*.PrefixExpression.expression.*, test_case.expression));
    }
}

test "should parse infix expression" {
    const TestCase = struct {
        content: []const u8,
        operator: Operator,
        left: Expression,
        right: Expression,
    };

    const test_cases = [_]TestCase{
        .{
            .content = "1 + 1",
            .operator = .Plus,
            .left = Expression{ .IntegerLiteral = 1 },
            .right = Expression{ .IntegerLiteral = 1 },
        },
        .{
            .content = "1.1 + 1.35",
            .operator = .Plus,
            .left = Expression{ .FloatLiteral = 1.1 },
            .right = Expression{ .FloatLiteral = 1.35 },
        },
        .{
            .content = "40 - 22",
            .operator = .Minus,
            .left = Expression{ .IntegerLiteral = 40 },
            .right = Expression{ .IntegerLiteral = 22 },
        },
        .{
            .content = "40.54 - 22.33",
            .operator = .Minus,
            .left = Expression{ .FloatLiteral = 40.54 },
            .right = Expression{ .FloatLiteral = 22.33 },
        },
        .{
            .content = "5 * 66",
            .operator = .Asterisk,
            .left = Expression{ .IntegerLiteral = 5 },
            .right = Expression{ .IntegerLiteral = 66 },
        },
        .{
            .content = "5.3 * 66.5",
            .operator = .Asterisk,
            .left = Expression{ .FloatLiteral = 5.3 },
            .right = Expression{ .FloatLiteral = 66.5 },
        },
        .{
            .content = "6 / 2",
            .operator = .Slash,
            .left = Expression{ .IntegerLiteral = 6 },
            .right = Expression{ .IntegerLiteral = 2 },
        },
        .{
            .content = "6.55 / 2.413",
            .operator = .Slash,
            .left = Expression{ .FloatLiteral = 6.55 },
            .right = Expression{ .FloatLiteral = 2.413 },
        },
        .{
            .content = "1 < 5",
            .operator = .Lt,
            .left = Expression{ .IntegerLiteral = 1 },
            .right = Expression{ .IntegerLiteral = 5 },
        },
        .{
            .content = "1 > 5",
            .operator = .Gt,
            .left = Expression{ .IntegerLiteral = 1 },
            .right = Expression{ .IntegerLiteral = 5 },
        },
        .{
            .content = "3 == 3",
            .operator = .Eq,
            .left = Expression{ .IntegerLiteral = 3 },
            .right = Expression{ .IntegerLiteral = 3 },
        },
        .{
            .content = "3 != 3",
            .operator = .NotEq,
            .left = Expression{ .IntegerLiteral = 3 },
            .right = Expression{ .IntegerLiteral = 3 },
        },
        .{
            .content = "true == false",
            .operator = .Eq,
            .left = Expression{ .BooleanLiteral = true },
            .right = Expression{ .BooleanLiteral = false },
        },
        .{
            .content = "true != false",
            .operator = .NotEq,
            .left = Expression{ .BooleanLiteral = true },
            .right = Expression{ .BooleanLiteral = false },
        },
    };

    for (test_cases) |test_case| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const expression = try getExpression(arena.allocator(), test_case.content);
        try expect(expression.* == .InfixExpression);
        try expect(expression.*.InfixExpression.operator == test_case.operator);
        try expect(std.meta.eql(expression.*.InfixExpression.left.*, test_case.left));
        try expect(std.meta.eql(expression.*.InfixExpression.right.*, test_case.right));
    }
}

test "function declaration" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected: []const u8,
        expected_error: ?ParserError,
    };

    const test_cases = [_]TestCase{
        .{
            .description = "empty function one line",
            .input =
            \\fnc test() {}
            ,
            .expected =
            \\fnc test() { }
            ,
            .expected_error = null,
        },
        .{
            .description = "empty function multi lines",
            .input =
            \\fnc test() {
            \\
            \\}
            ,
            .expected =
            \\fnc test() {
            \\
            \\}
            ,
            .expected_error = null,
        },
        .{
            .description = "one liner with params",
            .input =
            \\fnc test(a, b) { return a + b }
            ,
            .expected =
            \\fnc test(a, b) { return (a + b) }
            ,
            .expected_error = null,
        },
        .{
            .description = "multi liner with params and one statement",
            .input =
            \\fnc test(a, b) {
            \\    return a + b 
            \\}
            ,
            .expected =
            \\fnc test(a, b) {
            \\    return (a + b)
            \\}
            ,
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
            .expected =
            \\fnc test(a, b) {
            \\    x = (a * b)
            \\    y = (a / b)
            \\    return (x + y)
            \\}
            ,
            .expected_error = null,
        },
        .{
            .description = "multi liner with gaps",
            .input =
            \\fnc test(a, b) {
            \\    x = a * b
            \\    y = a / b
            \\    return x + y
            \\}
            ,
            .expected =
            \\fnc test(a, b) {
            \\    x = (a * b)
            \\    y = (a / b)
            \\    return (x + y)
            \\}
            ,
            .expected_error = null,
        },
        .{
            .description = "one liner without return",
            .input =
            \\fnc test(a, b) { a - b }
            ,
            .expected =
            \\fnc test(a, b) { (a - b) }
            ,
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
            .expected =
            \\fnc test(a, b) {
            \\    x = (a * b)
            \\    y = (a / b)
            \\    (x + y)
            \\}
            ,
            .expected_error = null,
        },
        .{
            .description = "new line before closing brace is needed for multi liner",
            .input =
            \\fnc test(a, b) {
            \\    x = a * b
            \\    y = a / b
            \\    x + y }
            ,
            .expected = "",
            .expected_error = ParserError.UnexpectedTokenType,
        },
        .{
            .description = "new line after opening brace is needed for multi liner",
            .input =
            \\fnc test(a, b) { x = a * b
            \\    y = a / b
            \\    x + y 
            \\}
            ,
            .expected = "",
            .expected_error = ParserError.UnexpectedTokenType,
        },
        .{
            .description = "multi liner with assignment on new line",
            .input =
            \\fnc test(a, b) {
            \\    x = a * b
            \\    y = a / b
            \\    x + y
            \\}
            \\
            \\result = test(a, b)
            ,
            .expected =
            \\fnc test(a, b) {
            \\    x = (a * b)
            \\    y = (a / b)
            \\    (x + y)
            \\}
            \\result = test(a, b)
            ,
            .expected_error = null,
        },
        .{
            .description = "multi liner with expression on new line",
            .input =
            \\fnc test(a, b) {
            \\    x = a * b
            \\    y = a / b
            \\    x + y
            \\}
            \\
            \\test(a, b)
            ,
            .expected =
            \\fnc test(a, b) {
            \\    x = (a * b)
            \\    y = (a / b)
            \\    (x + y)
            \\}
            \\test(a, b)
            ,
            .expected_error = null,
        },
    };

    std.debug.print("--- start function declaration tests ---\n", .{});
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
    std.debug.print("--- end function declaration tests ---\n", .{});
}

test "should parse function assignment" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\val = fnc test(a, b) {
        \\    return a + b
        \\}(1, 2)
    ;

    const expected_output =
        \\val = fnc test(a, b) {
        \\    return (a + b)
        \\}(1, 2)
    ;

    try expectFormattedProgram(arena.allocator(), content, expected_output);
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
        \\val = fnc test(a, b) {
        \\    return (a + b)
        \\}(1, 2)
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
        \\val = fnc test(a, b) { return (a + b) }(1, 2)
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
