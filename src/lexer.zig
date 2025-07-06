const std = @import("std");

pub const TokenType = enum {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Pipe,

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
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
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
        const charString: []const u8 = &.{char};

        const token = switch (char) {
            ',' => Token{ .type = TokenType.Comma, .literal = charString },
            '+' => Token{ .type = TokenType.Plus, .literal = charString },
            '-' => Token{ .type = TokenType.Minus, .literal = charString },
            '/' => Token{ .type = TokenType.Slash, .literal = charString },
            '*' => Token{ .type = TokenType.Asterisk, .literal = charString },
            '(' => Token{ .type = TokenType.LParen, .literal = charString },
            ')' => Token{ .type = TokenType.RParen, .literal = charString },
            '[' => Token{ .type = TokenType.LBracket, .literal = charString },
            ']' => Token{ .type = TokenType.RBracket, .literal = charString },
            '{' => Token{ .type = TokenType.LBrace, .literal = charString },
            '}' => Token{ .type = TokenType.RBrace, .literal = charString },
            '|' => Token{ .type = TokenType.Pipe, .literal = charString },
            '\n' => Token{ .type = TokenType.NewLine, .literal = charString },
            '<' => Token{ .type = TokenType.Lt, .literal = charString },
            '>' => Token{ .type = TokenType.Gt, .literal = charString },
            '!' => {
                return self.scan_bang_char();
            },
            '=' => {
                return self.scan_equal_char();
            },
            '0'...'9' => {
                return try self.scan_number();
            },
            '"' => {
                return try self.scan_string();
            },
            else => {
                return try self.scan_identifier();
            },
        };

        self.advance_pos();

        return token;
    }

    fn scan_bang_char(self: *Lexer) !Token {
        self.advance_pos();
        if (!self.is_end() and self.get_char() == '=') {
            self.advance_pos();
            return Token{ .type = TokenType.NotEq, .literal = "!=" };
        }
        return Token{ .type = TokenType.Bang, .literal = "!" };
    }

    fn scan_equal_char(self: *Lexer) !Token {
        self.advance_pos();
        if (!self.is_end() and self.get_char() == '=') {
            self.advance_pos();
            return Token{ .type = TokenType.Eq, .literal = "==" };
        }
        return Token{ .type = TokenType.Assign, .literal = "=" };
    }

    fn scan_string(self: *Lexer) !Token {
        self.advance_pos();

        const start = self.pos;
        var char = self.get_char();
        while (!self.is_end() and char != '"') : (char = self.get_char()) {
            self.advance_pos();
        }

        const result = self.buf[start..self.pos];
        const arena_copy = try self.arena.dupe(u8, result);

        self.advance_pos();

        return Token{ .type = TokenType.String, .literal = arena_copy };
    }

    fn scan_number(self: *Lexer) !Token {
        const start = self.pos;
        var dec_separator_found = false;
        var char = self.get_char();
        while (!self.is_end()) : (char = self.get_char()) {
            if (!std.ascii.isDigit(char) and (char != '.' or (char == '.' and dec_separator_found))) {
                break;
            }
            if (char == '.') {
                dec_separator_found = true;
            }
            self.advance_pos();
        }

        const result = self.buf[start..self.pos];
        const arena_copy = try self.arena.dupe(u8, result);

        if (dec_separator_found) {
            return Token{ .type = TokenType.Float, .literal = arena_copy };
        }
        return Token{ .type = TokenType.Integer, .literal = arena_copy };
    }

    fn scan_identifier(self: *Lexer) !Token {
        const start = self.pos;
        var char = self.get_char();
        while (!self.is_end() and Lexer.char_allowed_in_ident(char)) : (char = self.get_char()) {
            self.advance_pos();
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

    fn char_allowed_in_ident(char: u8) bool {
        return std.ascii.isAlphanumeric(char) or char == '_';
    }

    fn chopWhiteSpace(self: *Lexer) void {
        while (!self.is_end() and self.get_char() == ' ') {
            self.pos += 1;
        }
    }

    fn get_char(self: *Lexer) u8 {
        return self.buf[self.pos];
    }

    fn advance_pos(self: *Lexer) void {
        self.pos += 1;
    }

    fn is_end(self: *Lexer) bool {
        return self.pos >= self.buf.len;
    }
};
