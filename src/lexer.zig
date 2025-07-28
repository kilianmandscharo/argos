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

// TODO: implement col_x, col_y, row to give better debug info
pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    col_start: usize,
    col_end: usize,
    row: usize,

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
    row: usize,
    arena: std.mem.Allocator,

    pub fn init(arena: std.mem.Allocator, content: []const u8) !Lexer {
        if (content.len == 0) {
            return error.NoContent;
        }
        return Lexer{
            .buf = content,
            .pos = 0,
            .row = 0,
            .arena = arena,
        };
    }

    fn create_single_char_token(self: *Lexer, token_type: TokenType, literal: []const u8) Token {
        return Token{
            .type = token_type,
            .literal = literal,
            .col_start = self.pos,
            .col_end = self.pos + 1,
            .row = self.row,
        };
    }

    fn create_token(self: *Lexer, token_type: TokenType, literal: []const u8, start: usize, end: usize) Token {
        return Token{
            .type = token_type,
            .literal = literal,
            .col_start = start,
            .col_end = end,
            .row = self.row,
        };
    }

    pub fn next(self: *Lexer) !Token {
        self.chopWhiteSpace();

        if (self.pos == self.buf.len) {
            return self.create_single_char_token(.Eof, "EOF");
        }

        const char = self.buf[self.pos];

        const token = switch (char) {
            ',' => self.create_single_char_token(.Comma, ","),
            '+' => self.create_single_char_token(.Plus, "+"),
            '-' => self.create_single_char_token(.Minus, "-"),
            '/' => self.create_single_char_token(.Slash, "/"),
            '*' => self.create_single_char_token(.Asterisk, "*"),
            '(' => self.create_single_char_token(.LParen, "("),
            ')' => self.create_single_char_token(.RParen, ")"),
            '[' => self.create_single_char_token(.LBracket, "["),
            ']' => self.create_single_char_token(.RBracket, "]"),
            '{' => self.create_single_char_token(.LBrace, "{"),
            '}' => self.create_single_char_token(.RBrace, "}"),
            '<' => self.create_single_char_token(.Lt, "<"),
            '>' => self.create_single_char_token(.Gt, ">"),
            '\n' => newline: {
                const token = self.create_single_char_token(.NewLine, "<newline>");
                self.row += 1;
                break :newline token;
            },
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
            return self.create_token(.NotEq, "!=", self.pos, self.pos + 2);
        }
        return self.create_token(.Bang, "!", self.pos - 1, self.pos);
    }

    fn scanEqualChar(self: *Lexer) !Token {
        self.advancePos();
        if (self.getChar() == '=') {
            self.advancePos();
            return self.create_token(.Eq, "==", self.pos, self.pos + 2);
        }
        return self.create_token(.Assign, "=", self.pos - 1, self.pos);
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

        return self.create_token(.String, arena_copy, start, self.pos);
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
            return self.create_token(.Float, arena_copy, start, self.pos);
        }
        return self.create_token(.Integer, arena_copy, start, self.pos);
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
            return self.create_token(.False, arena_copy, start, self.pos);
        }

        if (std.mem.eql(u8, result, "true")) {
            return self.create_token(.True, arena_copy, start, self.pos);
        }

        if (std.mem.eql(u8, result, "if")) {
            return self.create_token(.If, arena_copy, start, self.pos);
        }

        if (std.mem.eql(u8, result, "else")) {
            return self.create_token(.Else, arena_copy, start, self.pos);
        }

        if (std.mem.eql(u8, result, "return")) {
            return self.create_token(.Return, arena_copy, start, self.pos);
        }

        if (std.mem.eql(u8, result, "fnc")) {
            return self.create_token(.Function, arena_copy, start, self.pos);
        }

        return self.create_token(.Identifier, arena_copy, start, self.pos);
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
