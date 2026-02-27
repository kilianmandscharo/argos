const std = @import("std");

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
    Print,
    Let,

    Error,
};

pub const Token = struct {
    source: []const u8,
    type: TokenType,
    start: usize,
    length: usize,
    line: usize,

    pub fn toString(self: *Token) []const u8 {
        return self.source[self.start .. self.start + self.length];
    }

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        try writer.print("{s} {s}", .{ @tagName(self.type), self.source[self.start .. self.start + self.length] });
    }
};

pub const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn makeToken(self: *Scanner, token_type: TokenType) Token {
        return Token{
            .source = self.source,
            .type = token_type,
            .start = self.start,
            .length = self.current - self.start,
            .line = self.line,
        };
    }

    pub fn errorToken(self: *Scanner, message: []const u8) Token {
        return Token{
            .source = message,
            .type = .Error,
            .start = 0,
            .length = message.len,
            .line = self.line,
        };
    }

    pub fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    pub fn isAtEnd(self: *Scanner) bool {
        return self.current == self.source.len - 1;
    }

    pub fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isAlpha(char: u8) bool {
        return std.ascii.isAlphabetic(char) or char == '_';
    }

    fn peek(self: *Scanner) u8 {
        return self.source[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.isAtEnd()) return '0';
        return self.source[self.current + 1];
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

    pub fn next(self: *Scanner) Token {
        self.chopWhiteSpace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.Eof);

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
                self.line += 1;
                return self.makeToken(.NewLine);
            },
            '-' => return if (self.match('>')) self.makeToken(.Arrow) else self.makeToken(.Minus),
            '!' => return if (self.match('=')) self.makeToken(.NotEq) else self.makeToken(.Bang),
            '<' => return if (self.match('=')) self.makeToken(.LtOrEq) else self.makeToken(.Lt),
            '>' => return if (self.match('=')) self.makeToken(.GtOrEq) else self.makeToken(.Gt),
            '=' => return if (self.match('=')) self.makeToken(.Eq) else self.makeToken(.Assign),
            '.' => return if (self.match('.')) self.makeToken(.DotDot) else self.makeToken(.Dot),
            '0'...'9' => return self.makeNumber(),
            '"' => return self.makeString(),
            else => {
                if (Scanner.isAlpha(char)) {
                    return self.makeIdentifier();
                }
                return self.errorToken("Unexpected character.");
            },
        }
    }

    fn makeString(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            _ = self.advance();
        }
        if (self.isAtEnd()) return self.errorToken("Unterminated string.");
        _ = self.advance();
        return self.makeToken(.String);
    }

    fn makeNumber(self: *Scanner) Token {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() != '.') return self.makeToken(.Int);
        if (std.ascii.isDigit(self.peekNext())) {
            _ = self.advance();
            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.makeToken(.Float);
    }

    fn makeIdentifier(self: *Scanner) Token {
        while (Scanner.isAlpha(self.peek()) or std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierType());
    }

    fn identifierType(self: *Scanner) TokenType {
        switch (self.source[self.start]) {
            'a' => return self.checkKeyword(1, "nd", .And),
            'l' => return self.checkKeyword(1, "et", .Let),
            'p' => return self.checkKeyword(1, "rint", .Print),
            'o' => return self.checkKeyword(1, "r", .Or),
            'e' => return self.checkKeyword(1, "lse", .Else),
            'n' => return self.checkKeyword(1, "ull", .Null),
            'r' => return self.checkKeyword(1, "eturn", .Return),
            't' => return self.checkKeyword(1, "rue", .True),
            'f' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'o' => return self.checkKeyword(2, "r", .For),
                        'a' => return self.checkKeyword(2, "lse", .False),
                        else => return .Identifier,
                    }
                }
            },
            'i' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'f' => return self.checkKeyword(2, "", .If),
                        'n' => return self.checkKeyword(2, "", .In),
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
        const matches = std.mem.eql(u8, self.source[start_index .. start_index + rest.len], rest);

        return if (matches) token_type else .Identifier;
    }
};
