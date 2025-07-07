const std = @import("std");
const expect = std.testing.expect;

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;
const Expression = parser_module.Expression;
const Operator = parser_module.Operator;

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
        .{ .content = "1 + 1", .operator = .Plus, .left = Expression{ .IntegerLiteral = 1 }, .right = Expression{ .IntegerLiteral = 1 } },
        .{ .content = "40 - 22", .operator = .Minus, .left = Expression{ .IntegerLiteral = 40 }, .right = Expression{ .IntegerLiteral = 22 } },
        .{ .content = "5 * 66", .operator = .Asterisk, .left = Expression{ .IntegerLiteral = 5 }, .right = Expression{ .IntegerLiteral = 66 } },
        .{ .content = "6 / 2", .operator = .Slash, .left = Expression{ .IntegerLiteral = 6 }, .right = Expression{ .IntegerLiteral = 2 } },
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
