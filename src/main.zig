const std = @import("std");

const TokenType = enum {
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

    Eq,
    NotEq,
    Bang,
    Identifier,
    Assign,
    Comma,

    NewLine,
    Eof,

    Plus,
    Minus,
    Divide,
    Multiply,
};

const Token = struct {
    type: TokenType,
    literal: []const u8,
};

const Lexer = struct {
    buf: []const u8,
    current_idx: usize,
    peek_idx: usize,
    gpa: std.mem.Allocator,
    arena: std.mem.Allocator,

    fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator, content: []const u8) !Lexer {
        if (content.len == 0) {
            return error.NoContent;
        }
        return Lexer{
            .buf = content,
            .current_idx = 0,
            .peek_idx = 1,
            .gpa = gpa,
            .arena = arena,
        };
    }

    fn next(self: *Lexer) !Token {
        self.chopWhiteSpace();

        if (self.current_idx == self.buf.len) {
            return Token{ .type = TokenType.Eof, .literal = "EOF" };
        }

        const char = self.buf[self.current_idx];
        const charString: []const u8 = &.{char};

        const token = switch (char) {
            ',' => Token{ .type = TokenType.Comma, .literal = charString },
            '+' => Token{ .type = TokenType.Plus, .literal = charString },
            '-' => Token{ .type = TokenType.Minus, .literal = charString },
            '/' => Token{ .type = TokenType.Divide, .literal = charString },
            '*' => Token{ .type = TokenType.Multiply, .literal = charString },
            '(' => Token{ .type = TokenType.LParen, .literal = charString },
            ')' => Token{ .type = TokenType.RParen, .literal = charString },
            '[' => Token{ .type = TokenType.LBracket, .literal = charString },
            ']' => Token{ .type = TokenType.RBracket, .literal = charString },
            '{' => Token{ .type = TokenType.LBrace, .literal = charString },
            '}' => Token{ .type = TokenType.RBrace, .literal = charString },
            '|' => Token{ .type = TokenType.Pipe, .literal = charString },
            '\n' => Token{ .type = TokenType.NewLine, .literal = charString },
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

        self.advance_idx();

        return token;
    }

    fn scan_bang_char(self: *Lexer) !Token {
        self.advance_idx();
        if (!self.has_reached_end() and self.get_current_char() == '=') {
            self.advance_idx();
            return Token{ .type = TokenType.NotEq, .literal = "!=" };
        }
        return Token{ .type = TokenType.Bang, .literal = "!" };
    }

    fn scan_equal_char(self: *Lexer) !Token {
        self.advance_idx();
        if (!self.has_reached_end() and self.get_current_char() == '=') {
            self.advance_idx();
            return Token{ .type = TokenType.Eq, .literal = "==" };
        }
        return Token{ .type = TokenType.Assign, .literal = "=" };
    }

    fn scan_string(self: *Lexer) !Token {
        var buf = std.ArrayList(u8).init(self.gpa);
        defer buf.deinit();

        self.advance_idx();

        var char = self.get_current_char();
        while (!self.has_reached_end() and char != '"') : (char = self.get_current_char()) {
            try buf.append(char);
            self.advance_idx();
        }

        self.advance_idx();

        const result = try buf.toOwnedSlice();
        defer self.gpa.free(result);

        const arena_copy = try self.arena.dupe(u8, result);

        return Token{ .type = TokenType.String, .literal = arena_copy };
    }

    fn scan_number(self: *Lexer) !Token {
        var buf = std.ArrayList(u8).init(self.gpa);
        defer buf.deinit();

        var dec_separator_found = false;
        var char = self.get_current_char();
        while (!self.has_reached_end()) : (char = self.get_current_char()) {
            if (!std.ascii.isDigit(char) and (char != '.' or (char == '.' and dec_separator_found))) {
                break;
            }
            if (char == '.') {
                dec_separator_found = true;
            }
            try buf.append(char);
            self.advance_idx();
        }

        const result = try buf.toOwnedSlice();
        defer self.gpa.free(result);

        const arena_copy = try self.arena.dupe(u8, result);

        return Token{ .type = TokenType.Integer, .literal = arena_copy };
    }

    fn scan_identifier(self: *Lexer) !Token {
        var buf = std.ArrayList(u8).init(self.gpa);
        defer buf.deinit();

        var char = self.get_current_char();
        while (!self.has_reached_end() and Lexer.char_allowed_in_ident(char)) : (char = self.get_current_char()) {
            try buf.append(char);
            self.advance_idx();
        }

        const result = try buf.toOwnedSlice();
        defer self.gpa.free(result);

        const arena_copy = try self.arena.dupe(u8, result);

        return Token{ .type = TokenType.Identifier, .literal = arena_copy };
    }

    fn char_allowed_in_ident(char: u8) bool {
        return std.ascii.isAlphanumeric(char) or char == '_';
    }

    fn chopWhiteSpace(self: *Lexer) void {
        while (self.current_idx < self.buf.len and self.buf[self.current_idx] == ' ') {
            self.current_idx += 1;
            self.peek_idx += 1;
        }
    }

    fn get_current_char(self: *Lexer) u8 {
        return self.buf[self.current_idx];
    }

    fn advance_idx(self: *Lexer) void {
        self.current_idx += 1;
        self.peek_idx += 1;
    }

    fn has_reached_end(self: *Lexer) bool {
        return self.current_idx >= self.buf.len;
    }

    fn can_peek(self: *Lexer) bool {
        return self.peek_idx < self.buf.len;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpaAllocator = gpa.allocator();
    defer {
        const status = gpa.deinit();
        if (status != .ok) {
            std.debug.print("Memory leak detected: {}\n", .{status});
        }
    }

    const file = try std.fs.cwd().openFile("test.argos", .{});
    defer file.close();

    const content = try file.readToEndAlloc(gpaAllocator, std.math.maxInt(usize));
    defer gpaAllocator.free(content);

    var arena = std.heap.ArenaAllocator.init(gpaAllocator);
    const arenaAllocator = arena.allocator();
    defer arena.deinit();

    var lexer = try Lexer.init(gpaAllocator, arenaAllocator, content);

    while (true) {
        const token = try lexer.next();
        std.debug.print("token: {any} --> {s}\n", .{ token.type, token.literal });
        if (token.type == TokenType.Eof) {
            break;
        }
    }
}
