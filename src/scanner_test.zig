const std = @import("std");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");

fn assertTokenEquals(expected_type: scanner.TokenType, literal: []const u8, got: scanner.Token) !void {
    std.testing.expect(expected_type == got.type) catch |err| {
        std.debug.print("expected {any}, got {any}\n", .{ expected_type, got.type });
        return err;
    };
    try std.testing.expectEqualStrings(literal, got.toString());
}

test "scanner" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
    };

    const run = struct {
        fn runTest(test_case: TestCase) anyerror!void {
            var s = scanner.Scanner.init(test_case.input);

            try assertTokenEquals(.Identifier, "a", try s.next());
            try assertTokenEquals(.Assign, "=", try s.next());
            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Identifier, "b", try s.next());
            try assertTokenEquals(.Assign, "=", try s.next());
            try assertTokenEquals(.LParen, "(", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.Plus, "+", try s.next());
            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.RParen, ")", try s.next());
            try assertTokenEquals(.Slash, "/", try s.next());
            try assertTokenEquals(.Int, "3", try s.next());
            try assertTokenEquals(.Asterisk, "*", try s.next());
            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Minus, "-", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.True, "true", try s.next());
            try assertTokenEquals(.Eq, "==", try s.next());
            try assertTokenEquals(.True, "true", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.False, "false", try s.next());
            try assertTokenEquals(.NotEq, "!=", try s.next());
            try assertTokenEquals(.True, "true", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Lt, "<", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Gt, ">", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Bang, "!", try s.next());
            try assertTokenEquals(.True, "true", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.LtOrEq, "<=", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.GtOrEq, ">=", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.LeftShift, "<<", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.RightShift, ">>", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.True, "true", try s.next());
            try assertTokenEquals(.Or, "or", try s.next());
            try assertTokenEquals(.False, "false", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.True, "true", try s.next());
            try assertTokenEquals(.And, "and", try s.next());
            try assertTokenEquals(.False, "false", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Ampersand, "&", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Pipe, "|", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Caret, "^", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Tilde, "~", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());
            try assertTokenEquals(.NewLine, "<newline>", try s.next());

            try assertTokenEquals(.Int, "1", try s.next());
            try assertTokenEquals(.Percent, "%", try s.next());
            try assertTokenEquals(.Int, "5", try s.next());

            try assertTokenEquals(.Eof, "EOF", try s.next());
        }
    }.runTest;

    const test_cases = [_]TestCase{
        .{
            .description = "tokenize",
            .input =
            \\a = 1
            \\b = (5 + 1) / 3 * 1 - 5
            \\true == true
            \\false != true
            \\1 < 5
            \\1 > 5
            \\!true
            \\1 <= 5
            \\1 >= 5
            \\1 << 5
            \\1 >> 5
            \\true or false
            \\true and false
            \\1 & 5
            \\1 | 5
            \\1 ^ 5
            \\1 ~ 5
            \\1 % 5
            ,
        },
    };

    try test_utils.runTests(TestCase, "tokenize", &test_cases, run);
}
