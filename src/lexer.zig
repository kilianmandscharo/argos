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

    Identifier,
    Assign,
    Comma,

    Bang,
    Lt,
    Gt,
    Eq,
    NotEq,
    Plus,
    Minus,
    Slash,
    Asterisk,

    Return,
    If,
    Else,
    Function,

    NewLine,
    Eof,
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        try writer.print("Token {any} --> {s}", .{ self.type, self.literal });
    }
};

pub const Lexer = struct {
    buf: []const u8,
    pos: usize,
    arena: std.mem.Allocator,

    pub fn init(arena: std.mem.Allocator, content: []const u8) !Lexer {
        if (content.len == 0) {
            return error.NoContent;
        }
        return Lexer{
            .buf = content,
            .pos = 0,
            .arena = arena,
        };
    }

    pub fn next(self: *Lexer) !Token {
        self.chopWhiteSpace();

        if (self.pos == self.buf.len) {
            return Token{ .type = TokenType.Eof, .literal = "EOF" };
        }

        const char = self.buf[self.pos];

        const token = switch (char) {
            ',' => Token{ .type = TokenType.Comma, .literal = "," },
            '+' => Token{ .type = TokenType.Plus, .literal = "+" },
            '-' => Token{ .type = TokenType.Minus, .literal = "-" },
            '/' => Token{ .type = TokenType.Slash, .literal = "/" },
            '*' => Token{ .type = TokenType.Asterisk, .literal = "*" },
            '(' => Token{ .type = TokenType.LParen, .literal = "(" },
            ')' => Token{ .type = TokenType.RParen, .literal = ")" },
            '[' => Token{ .type = TokenType.LBracket, .literal = "[" },
            ']' => Token{ .type = TokenType.RBracket, .literal = "]" },
            '{' => Token{ .type = TokenType.LBrace, .literal = "{" },
            '}' => Token{ .type = TokenType.RBrace, .literal = "}" },
            '\n' => Token{ .type = TokenType.NewLine, .literal = "<newline>" },
            '<' => Token{ .type = TokenType.Lt, .literal = "<" },
            '>' => Token{ .type = TokenType.Gt, .literal = ">" },
            '!' => {
                return self.scanBangChar();
            },
            '=' => {
                return self.scanEqualChar();
            },
            '0'...'9' => {
                return try self.scanNumber();
            },
            '"' => {
                return try self.scanString();
            },
            else => {
                return try self.scanIdentifier();
            },
        };

        self.advancePos();

        return token;
    }

    fn scanBangChar(self: *Lexer) !Token {
        self.advancePos();
        if (self.getChar() == '=') {
            self.advancePos();
            return Token{ .type = TokenType.NotEq, .literal = "!=" };
        }
        return Token{ .type = TokenType.Bang, .literal = "!" };
    }

    fn scanEqualChar(self: *Lexer) !Token {
        self.advancePos();
        if (self.getChar() == '=') {
            self.advancePos();
            return Token{ .type = TokenType.Eq, .literal = "==" };
        }
        return Token{ .type = TokenType.Assign, .literal = "=" };
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
        const arena_copy = try self.arena.dupe(u8, result);

        self.advancePos();

        return Token{ .type = TokenType.String, .literal = arena_copy };
    }

    fn scanNumber(self: *Lexer) !Token {
        const start = self.pos;
        var dec_separator_found = false;
        while (self.getChar()) |char| {
            if (!std.ascii.isDigit(char) and (char != '.' or (char == '.' and dec_separator_found))) {
                break;
            }
            if (char == '.') {
                dec_separator_found = true;
            }
            self.advancePos();
        }

        const result = self.buf[start..self.pos];
        const arena_copy = try self.arena.dupe(u8, result);

        if (dec_separator_found) {
            return Token{ .type = TokenType.Float, .literal = arena_copy };
        }
        return Token{ .type = TokenType.Integer, .literal = arena_copy };
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
        const arena_copy = try self.arena.dupe(u8, result);

        if (std.mem.eql(u8, result, "false")) {
            return Token{ .type = TokenType.False, .literal = arena_copy };
        }

        if (std.mem.eql(u8, result, "true")) {
            return Token{ .type = TokenType.True, .literal = arena_copy };
        }

        if (std.mem.eql(u8, result, "if")) {
            return Token{ .type = TokenType.If, .literal = arena_copy };
        }

        if (std.mem.eql(u8, result, "else")) {
            return Token{ .type = TokenType.Else, .literal = arena_copy };
        }

        if (std.mem.eql(u8, result, "return")) {
            return Token{ .type = TokenType.Return, .literal = arena_copy };
        }

        if (std.mem.eql(u8, result, "fnc")) {
            return Token{ .type = TokenType.Function, .literal = arena_copy };
        }

        return Token{ .type = TokenType.Identifier, .literal = arena_copy };
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

    fn advancePos(self: *Lexer) void {
        self.pos += 1;
    }

    fn isEnd(self: *Lexer) bool {
        return self.pos >= self.buf.len;
    }
};
