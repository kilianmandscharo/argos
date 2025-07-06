const std = @import("std");
const expect = std.testing.expect;

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;
const Token = lexer_module.Token;

fn assertTokenEquals(expected: Token, got: Token) !void {
    expect(expected.type == got.type) catch |err| {
        std.debug.print("expected {any}, got {any}", .{ expected.type, got.type });
        return err;
    };

    expect(std.mem.eql(u8, expected.literal, got.literal)) catch |err| {
        std.debug.print("expected {s}, got {s}", .{ expected.literal, got.literal });
        return err;
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

    try assertTokenEquals(Token{ .type = .Identifier, .literal = "a" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Assign, .literal = "=" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .Identifier, .literal = "b" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Assign, .literal = "=" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .LParen, .literal = "(" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Plus, .literal = "+" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .RParen, .literal = ")" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Slash, .literal = "/" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "3" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Asterisk, .literal = "*" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Minus, .literal = "-" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .Function, .literal = "fnc" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .LParen, .literal = "(" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Identifier, .literal = "first" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Identifier, .literal = "second" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .RParen, .literal = ")" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .LBrace, .literal = "{" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Return, .literal = "return" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Identifier, .literal = "first" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Plus, .literal = "+" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Identifier, .literal = "second" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .RBrace, .literal = "}" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .Identifier, .literal = "arr" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Assign, .literal = "=" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .LBracket, .literal = "[" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "100" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Float, .literal = "3.1415" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    try assertTokenEquals(Token{ .type = .String, .literal = "hello world" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Comma, .literal = "," }, try lexer.next());
    try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .RBracket, .literal = "]" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Eq, .literal = "==" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .False, .literal = "false" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NotEq, .literal = "!=" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Lt, .literal = "<" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Gt, .literal = ">" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "5" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .Bang, .literal = "!" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .True, .literal = "true" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .If, .literal = "if" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Identifier, .literal = "x" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Lt, .literal = "<" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Identifier, .literal = "y" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .LBrace, .literal = "{" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Return, .literal = "return" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "1" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .RBrace, .literal = "}" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Else, .literal = "else" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .LBrace, .literal = "{" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Return, .literal = "return" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .Integer, .literal = "2" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .NewLine, .literal = "\n" }, try lexer.next());
    try assertTokenEquals(Token{ .type = .RBrace, .literal = "}" }, try lexer.next());

    try assertTokenEquals(Token{ .type = .Eof, .literal = "EOF" }, try lexer.next());
}
