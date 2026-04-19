const std = @import("std");
const test_utils = @import("test_utils.zig");
const ast = @import("ast.zig");
const vm = @import("vm.zig");

pub const TokenType = enum {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Assign,
    Comma,

    String,
    Float,
    Int,
    True,
    False,
    Null,
    Identifier,

    Bang,
    Lt,
    LtOrEq,
    Gt,
    GtOrEq,
    Eq,
    NotEq,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Percent,

    Return,
    For,
    Dot,
    DotDot,
    Arrow,

    NewLine,
    Eof,

    And,
    Or,

    Pipe,
    Ampersand,
    Caret,
    Tilde,
    LeftShift,
    RightShift,
    Let,
    Match,
    While,
    Fn,
    List,
    Table,

    pub fn toString(self: @This()) []const u8 {
        return @tagName(self);
    }
};

pub const Token = struct {
    data: []const u8,
    type: TokenType,
    line: u32,
    column: u32,

    pub fn dummy() @This() {
        return .{
            .data = "",
            .type = .Eof,
            .line = 0,
            .column = 0,
        };
    }

    pub fn toString(self: Token) []const u8 {
        if (self.type == .NewLine) return "<newline>";
        if (self.type == .Eof) return "EOF";
        return self.data;
    }

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        try writer.print("{s}({s}:{d}:{d})", .{ @tagName(self.type), self.data, self.line + 1, self.column + 1 });
    }

    pub fn printError(
        self: @This(),
        message: []const u8,
        script_context: *vm.ScriptContext,
        module: []const u8,
    ) void {
        const line = script_context.lines.items[self.line];
        var end = line.end;

        if (end == 0) {
            end = line.start;
            while (end < script_context.source.len and script_context.source[end] != '\n') {
                end += 1;
            }
        }

        std.debug.print("{s}:{d}:{d}: error: {s}\n{s}\n", .{
            script_context.file_name,
            line.no + 1,
            self.column + 1,
            message,
            script_context.source[line.start .. end - 1],
        });

        for (0..self.column) |_| {
            std.debug.print(" ", .{});
        }

        for (0..self.data.len) |_| {
            std.debug.print("^", .{});
        }

        std.debug.print("\n{s}Error\n", .{module});
    }
};

pub const Line = struct {
    start: u32,
    end: u32,
    no: u32,

    pub fn init(start: u32, no: u32) @This() {
        return .{
            .start = start,
            .end = 0,
            .no = no,
        };
    }
};

pub const Lines = *std.ArrayList(Line);

pub const Scanner = struct {
    start: usize,
    current: usize,
    column: u32,
    arena: std.mem.Allocator,
    script_context: *vm.ScriptContext,

    pub fn init(arena: std.mem.Allocator, script_context: *vm.ScriptContext) !Scanner {
        try script_context.lines.append(arena, .init(0, 0));

        return .{
            .arena = arena,
            .start = 0,
            .current = 0,
            .column = 0,
            .script_context = script_context,
        };
    }

    pub fn makeToken(self: *Scanner, token_type: TokenType) Token {
        return .{
            .data = self.script_context.source[self.start..self.current],
            .type = token_type,
            .line = self.currentLine().no,
            .column = self.column - @as(u32, @intCast(self.current - self.start)),
        };
    }

    fn currentLine(self: *Scanner) *Line {
        return &self.script_context.lines.items[self.script_context.lines.items.len - 1];
    }

    fn scannerError(self: *Scanner, message: []const u8) error{ScannerError} {
        std.debug.print("Scanner error at line {d}: {s}\n", .{ self.currentLine().no + 1, message });
        return error.ScannerError;
    }

    pub fn advance(self: *Scanner) u8 {
        self.column += 1;
        self.current += 1;
        return self.script_context.source[self.current - 1];
    }

    pub fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.script_context.source.len;
    }

    pub fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.script_context.source[self.current] != expected) return false;
        _ = self.advance();
        return true;
    }

    fn isAlpha(char: u8) bool {
        return std.ascii.isAlphabetic(char) or char == '_';
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return '0';
        return self.script_context.source[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.isAtEnd()) return '0';
        return self.script_context.source[self.current + 1];
    }

    fn chopWhiteSpace(self: *Scanner) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                else => return,
            }
        }
    }

    fn endLine(self: *Scanner) u32 {
        const line = self.currentLine();
        line.end = @intCast(self.current);
        return line.no + 1;
    }

    pub fn next(self: *Scanner) !Token {
        self.chopWhiteSpace();
        self.start = self.current;

        if (self.isAtEnd()) {
            _ = self.endLine();
            return self.makeToken(.Eof);
        }

        const char = self.advance();

        switch (char) {
            '(' => return self.makeToken(.LParen),
            ')' => return self.makeToken(.RParen),
            '{' => return self.makeToken(.LBrace),
            '}' => return self.makeToken(.RBrace),
            ',' => return self.makeToken(.Comma),
            '+' => return self.makeToken(.Plus),
            '/' => return self.makeToken(.Slash),
            '*' => return self.makeToken(.Asterisk),
            '[' => return self.makeToken(.LBracket),
            ']' => return self.makeToken(.RBracket),
            '&' => return self.makeToken(.Ampersand),
            '|' => return self.makeToken(.Pipe),
            '^' => return self.makeToken(.Caret),
            '~' => return self.makeToken(.Tilde),
            '%' => return self.makeToken(.Percent),
            '\n' => {
                const token = self.makeToken(.NewLine);
                const next_line_no = self.endLine();
                try self.script_context.lines.append(self.arena, .init(@intCast(self.current), next_line_no));
                self.column = 0;
                return token;
            },
            '-' => return if (self.match('>')) self.makeToken(.Arrow) else self.makeToken(.Minus),
            '!' => return if (self.match('=')) self.makeToken(.NotEq) else self.makeToken(.Bang),
            '<' => return self.scanLess(),
            '>' => return self.scanGreater(),
            '=' => return if (self.match('=')) self.makeToken(.Eq) else self.makeToken(.Assign),
            '.' => return if (self.match('.')) self.makeToken(.DotDot) else self.makeToken(.Dot),
            '0'...'9' => return self.makeNumber(),
            '"' => return self.makeString(),
            else => {
                if (Scanner.isAlpha(char)) {
                    return self.makeIdentifier();
                }
                return self.scannerError("Unexpected character.");
            },
        }
    }

    fn scanLess(self: *Scanner) !Token {
        if (self.match('=')) return self.makeToken(.LtOrEq);
        if (self.match('<')) return self.makeToken(.LeftShift);
        return self.makeToken(.Lt);
    }

    fn scanGreater(self: *Scanner) !Token {
        if (self.match('=')) return self.makeToken(.GtOrEq);
        if (self.match('>')) return self.makeToken(.RightShift);
        return self.makeToken(.Gt);
    }

    fn makeString(self: *Scanner) !Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            _ = self.advance();
        }
        if (self.isAtEnd()) return self.scannerError("Unterminated string.");
        _ = self.advance();
        return self.makeToken(.String);
    }

    fn makeNumber(self: *Scanner) Token {
        while (std.ascii.isDigit(self.peek()) and !self.isAtEnd()) {
            _ = self.advance();
        }
        if (self.peek() != '.' or self.peekNext() == '.') return self.makeToken(.Int);
        _ = self.advance();
        while (std.ascii.isDigit(self.peek()) and !self.isAtEnd()) {
            _ = self.advance();
        }
        return self.makeToken(.Float);
    }

    fn makeIdentifier(self: *Scanner) Token {
        while (!self.isAtEnd() and (Scanner.isAlpha(self.peek()) or std.ascii.isDigit(self.peek()))) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        switch (self.script_context.source[self.start]) {
            'a' => return self.checkKeyword(1, "nd", .And),
            'L' => return self.checkKeyword(1, "ist", .List),
            'l' => return self.checkKeyword(1, "et", .Let),
            'w' => return self.checkKeyword(1, "hile", .While),
            'm' => return self.checkKeyword(1, "atch", .Match),
            'o' => return self.checkKeyword(1, "r", .Or),
            'n' => return self.checkKeyword(1, "ull", .Null),
            'r' => return self.checkKeyword(1, "eturn", .Return),
            't' => return self.checkKeyword(1, "rue", .True),
            'T' => return self.checkKeyword(1, "able", .Table),
            'f' => {
                if (self.current - self.start > 1) {
                    switch (self.script_context.source[self.start + 1]) {
                        'o' => return self.checkKeyword(2, "r", .For),
                        'a' => return self.checkKeyword(2, "lse", .False),
                        'n' => return self.checkKeyword(2, "", .Fn),
                        else => return .Identifier,
                    }
                }
            },
            else => return .Identifier,
        }
        return .Identifier;
    }

    fn checkKeyword(self: *Scanner, start: usize, rest: []const u8, token_type: TokenType) TokenType {
        const has_correct_length = self.current - self.start == start + rest.len;
        if (!has_correct_length) return .Identifier;

        const start_index = self.start + start;
        const matches = std.mem.eql(u8, self.script_context.source[start_index .. start_index + rest.len], rest);

        return if (matches) token_type else .Identifier;
    }
};
