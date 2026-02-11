const std = @import("std");

pub const TokenType = enum {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    String,
    Integer,
    Float,
    True,
    False,
    Null,

    Identifier,
    Assign,
    Comma,

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
    If,
    Else,
    For,
    In,
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
};

// TODO: implement col_x, col_y, row to give better debug info
pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        try writer.print("Token {any} '{s}'", .{ self.type, self.literal });
    }
};

const keywords = std.StaticStringMap(Token).initComptime(.{
    .{ "false", Token{ .type = .False, .literal = "false" } },
    .{ "true", Token{ .type = .True, .literal = "true" } },
    .{ "if", Token{ .type = .If, .literal = "if" } },
    .{ "else", Token{ .type = .Else, .literal = "else" } },
    .{ "return", Token{ .type = .Return, .literal = "return" } },
    .{ "in", Token{ .type = .In, .literal = "in" } },
    .{ "for", Token{ .type = .For, .literal = "for" } },
    .{ "or", Token{ .type = .Or, .literal = "or" } },
    .{ "and", Token{ .type = .And, .literal = "and" } },
    .{ "null", Token{ .type = .Null, .literal = "null" } },
});

pub const Lexer = struct {
    buf: []const u8,
    pos: usize,
    static_lifetime_arena: std.mem.Allocator,

    pub fn init(static_lifetime_arena: std.mem.Allocator, content: []const u8) !Lexer {
        if (content.len == 0) {
            return error.NoContent;
        }
        return Lexer{
            .buf = content,
            .pos = 0,
            .static_lifetime_arena = static_lifetime_arena,
        };
    }

    pub fn next(self: *Lexer) !Token {
        self.chopWhiteSpace();

        if (self.pos == self.buf.len) {
            return Token{ .type = .Eof, .literal = "EOF" };
        }

        const char = self.buf[self.pos];

        const token = switch (char) {
            ',' => Token{ .type = .Comma, .literal = "," },
            '+' => Token{ .type = .Plus, .literal = "+" },
            '/' => Token{ .type = .Slash, .literal = "/" },
            '*' => Token{ .type = .Asterisk, .literal = "*" },
            '(' => Token{ .type = .LParen, .literal = "(" },
            ')' => Token{ .type = .RParen, .literal = ")" },
            '[' => Token{ .type = .LBracket, .literal = "[" },
            ']' => Token{ .type = .RBracket, .literal = "]" },
            '{' => Token{ .type = .LBrace, .literal = "{" },
            '}' => Token{ .type = .RBrace, .literal = "}" },
            '\n' => Token{ .type = .NewLine, .literal = "<newline>" },
            '&' => Token{ .type = .Ampersand, .literal = "&" },
            '|' => Token{ .type = .Pipe, .literal = "|" },
            '^' => Token{ .type = .Caret, .literal = "^" },
            '~' => Token{ .type = .Tilde, .literal = "~" },
            '%' => Token{ .type = .Percent, .literal = "%" },
            '-' => return self.scanMinus(),
            '<' => return self.scanLt(),
            '>' => return self.scanGt(),
            '.' => return self.scanDot(),
            '!' => return self.scanBangChar(),
            '=' => return self.scanEqualChar(),
            '0'...'9' => return try self.scanNumber(),
            '"' => return try self.scanString(),
            else => return try self.scanIdentifier(),
        };

        self.advancePos();

        return token;
    }

    fn scanMinus(self: *Lexer) Token {
        self.advancePos();
        switch (self.getChar().?) {
            '>' => {
                self.advancePos();
                return Token{ .type = .Arrow, .literal = "->" };
            },
            else => return Token{ .type = .Minus, .literal = "-" },
        }
    }

    fn scanLt(self: *Lexer) Token {
        self.advancePos();
        switch (self.getChar().?) {
            '<' => {
                self.advancePos();
                return Token{ .type = .LeftShift, .literal = "<<" };
            },
            '=' => {
                self.advancePos();
                return Token{ .type = .LtOrEq, .literal = "<=" };
            },
            else => return Token{ .type = .Lt, .literal = "<" },
        }
    }

    fn scanGt(self: *Lexer) !Token {
        self.advancePos();
        switch (self.getChar().?) {
            '>' => {
                self.advancePos();
                return Token{ .type = .RightShift, .literal = ">>" };
            },
            '=' => {
                self.advancePos();
                return Token{ .type = .GtOrEq, .literal = ">=" };
            },
            else => return Token{ .type = .Gt, .literal = ">" },
        }
    }

    fn scanDot(self: *Lexer) !Token {
        self.advancePos();
        if (self.getChar() == '.') {
            self.advancePos();
            return Token{ .type = .DotDot, .literal = ".." };
        }
        return Token{ .type = .Dot, .literal = "." };
    }

    fn scanBangChar(self: *Lexer) !Token {
        self.advancePos();
        if (self.getChar() == '=') {
            self.advancePos();
            return Token{ .type = .NotEq, .literal = "!=" };
        }
        return Token{ .type = .Bang, .literal = "!" };
    }

    fn scanEqualChar(self: *Lexer) !Token {
        self.advancePos();
        if (self.getChar() == '=') {
            self.advancePos();
            return Token{ .type = .Eq, .literal = "==" };
        }
        return Token{ .type = .Assign, .literal = "=" };
    }

    fn scanString(self: *Lexer) !Token {
        self.advancePos();

        const start = self.pos;
        while (self.getChar()) |char| {
            if (char == '"') {
                break;
            }
            self.advancePos();
        }

        const result = self.buf[start..self.pos];
        const arena_copy = try self.static_lifetime_arena.dupe(u8, result);

        self.advancePos();

        return Token{ .type = .String, .literal = arena_copy };
    }

    fn scanNumber(self: *Lexer) !Token {
        const start = self.pos;
        var dec_separator_found = false;
        while (self.getChar()) |char| {
            if (!std.ascii.isDigit(char) and (char != '.' or (char == '.' and dec_separator_found))) {
                break;
            }
            if (char == '.') {
                if (self.peekChar().? == '.') {
                    break;
                }
                dec_separator_found = true;
            }
            self.advancePos();
        }

        const result = self.buf[start..self.pos];
        const arena_copy = try self.static_lifetime_arena.dupe(u8, result);

        if (dec_separator_found) {
            return Token{ .type = .Float, .literal = arena_copy };
        }
        return Token{ .type = .Integer, .literal = arena_copy };
    }

    fn scanIdentifier(self: *Lexer) !Token {
        const start = self.pos;
        while (self.getChar()) |char| {
            if (!Lexer.charAllowedInIdent(char)) {
                break;
            }
            self.advancePos();
        }

        const result = self.buf[start..self.pos];

        if (keywords.get(result)) |token| {
            return token;
        }

        const arena_copy = try self.static_lifetime_arena.dupe(u8, result);
        return Token{ .type = .Identifier, .literal = arena_copy };
    }

    fn charAllowedInIdent(char: u8) bool {
        return std.ascii.isAlphanumeric(char) or char == '_';
    }

    fn chopWhiteSpace(self: *Lexer) void {
        while (self.getChar() == ' ') {
            self.pos += 1;
        }
    }

    fn getChar(self: *Lexer) ?u8 {
        if (self.pos >= self.buf.len) {
            return null;
        }
        return self.buf[self.pos];
    }

    fn peekChar(self: *Lexer) ?u8 {
        if (self.pos + 1 >= self.buf.len) {
            return null;
        }
        return self.buf[self.pos + 1];
    }

    fn advancePos(self: *Lexer) void {
        self.pos += 1;
    }

    fn isEnd(self: *Lexer) bool {
        return self.pos >= self.buf.len;
    }
};
