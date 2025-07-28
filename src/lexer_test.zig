const std = @import("std");
const expect = std.testing.expect;

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;
const Token = lexer_module.Token;
const TokenType = lexer_module.TokenType;

fn assertTokenEquals(expected: Token, got: Token) !void {
    expect(expected.type == got.type) catch |err| {
        std.debug.print("expected {any}, got {any}\n", .{ expected.type, got.type });
        return err;
    };
    expect(expected.col_start == got.col_start) catch |err| {
        std.debug.print("expected {any}, got {any}\n", .{ expected.col_start, got.col_start });
        return err;
    };
    expect(expected.col_end == got.col_end) catch |err| {
        std.debug.print("expected {any}, got {any}\n", .{ expected.col_end, got.col_end });
        return err;
    };
    expect(expected.row == got.row) catch |err| {
        std.debug.print("expected {any}, got {any}\n", .{ expected.row, got.row });
        return err;
    };
    expect(std.mem.eql(u8, expected.literal, got.literal)) catch |err| {
        std.debug.print("expected {s}, got {s}\n", .{ expected.literal, got.literal });
        return err;
    };
}

fn create_token(token_type: TokenType, literal: []const u8, start: usize, end: usize, row: usize) Token {
    return Token{
        .type = token_type,
        .literal = literal,
        .col_start = start,
        .col_end = end,
        .row = row,
    };
}

test "should tokenize" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const content =
        \\a = 1
        \\b = (5 + 1) / 3 * 1 - 5
        \\fnc(first, second) {
        \\    return first + second
        \\}
        \\arr = [100, 3.1415, "hello world", true]
        \\true == true
        \\false != true
        \\1 < 5
        \\1 > 5
        \\!true
        \\if x < y {
        \\    return 1
        \\} else {
        \\    return 2
        \\}
    ;

    var lexer = try Lexer.init(arena.allocator(), content);

    try assertTokenEquals(create_token(.Identifier, "a", 0, 1, 0), try lexer.next());
    try assertTokenEquals(create_token(.Assign, "=", 2, 3, 0), try lexer.next());
    try assertTokenEquals(create_token(.Integer, "1", 4, 5, 0), try lexer.next());
    try assertTokenEquals(create_token(.NewLine, "<newline>", 5, 6, 0), try lexer.next());

    // TODO: finish
    try assertTokenEquals(create_token(.Identifier, "b", 0, 1, 1), try lexer.next());
    // try assertTokenEquals(Token{ .type = .Assign, .literal = "=" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .LParen, .literal = "(" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Plus, .literal = "+" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .RParen, .literal = ")" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Slash, .literal = "/" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "3" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Asterisk, .literal = "*" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Minus, .literal = "-" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .Function, .literal = "fnc" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .LParen, .literal = "(" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Identifier, .literal = "first" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Identifier, .literal = "second" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .RParen, .literal = ")" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .LBrace, .literal = "{" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Return, .literal = "return" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Identifier, .literal = "first" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Plus, .literal = "+" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Identifier, .literal = "second" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .RBrace, .literal = "}" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .Identifier, .literal = "arr" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Assign, .literal = "=" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .LBracket, .literal = "[" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "100" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Float, .literal = "3.1415" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .String, .literal = "hello world" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .RBracket, .literal = "]" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Eq, .literal = "==" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .False, .literal = "false" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NotEq, .literal = "!=" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Lt, .literal = "<" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Gt, .literal = ">" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .Bang, .literal = "!" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .If, .literal = "if" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Identifier, .literal = "x" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Lt, .literal = "<" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Identifier, .literal = "y" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .LBrace, .literal = "{" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Return, .literal = "return" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .RBrace, .literal = "}" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Else, .literal = "else" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .LBrace, .literal = "{" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Return, .literal = "return" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .Integer, .literal = "2" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    // try assertTokenEquals(Token{ .type = .RBrace, .literal = "}" }, try lexer.next());
    //
    // try assertTokenEquals(Token{ .type = .Eof, .literal = "EOF" }, try lexer.next());
}
