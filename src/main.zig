const std = @import("std");

const TokenType = enum {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    String,
    Integer,
    Float,

    Identifier,

    Equals,

    NewLine,
    Eof,
};

const Token = struct {
    type: TokenType,
    literal: []const u8,
};

const Lexer = struct {
    buf: []const u8,
    current_idx: usize,
    peek_idx: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, content: []const u8) !Lexer {
        if (content.len == 0) {
            return error.NoContent;
        }
        return Lexer{
            .buf = content,
            .current_idx = 0,
            .peek_idx = 1,
            .allocator = allocator,
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
            '(' => Token{ .type = TokenType.LParen, .literal = charString },
            ')' => Token{ .type = TokenType.RParen, .literal = charString },
            '[' => Token{ .type = TokenType.LBracket, .literal = charString },
            ']' => Token{ .type = TokenType.RBracket, .literal = charString },
            '{' => Token{ .type = TokenType.LBrace, .literal = charString },
            '}' => Token{ .type = TokenType.RBrace, .literal = charString },
            '=' => Token{ .type = TokenType.Equals, .literal = charString },
            '\n' => Token{ .type = TokenType.NewLine, .literal = charString },
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

        self.current_idx += 1;
        self.peek_idx += 1;

        return token;
    }

    fn scanString(self: *Lexer) !Token {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        self.current_idx += 1;
        self.peek_idx += 1;

        while (self.current_idx < self.buf.len and self.buf[self.current_idx] != '"') {
            try buf.append(self.buf[self.current_idx]);
            self.current_idx += 1;
            self.peek_idx += 1;
        }

        self.current_idx += 1;
        self.peek_idx += 1;

        const ident: []u8 = try buf.toOwnedSlice();
        const const_ident: []const u8 = ident;

        return Token{ .type = TokenType.String, .literal = const_ident };
    }

    fn scanNumber(self: *Lexer) !Token {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        while (self.current_idx < self.buf.len and std.ascii.isDigit(self.buf[self.current_idx])) {
            try buf.append(self.buf[self.current_idx]);
            self.current_idx += 1;
            self.peek_idx += 1;
        }

        const ident: []u8 = try buf.toOwnedSlice();
        const const_ident: []const u8 = ident;

        return Token{ .type = TokenType.Integer, .literal = const_ident };
    }

    fn scanIdentifier(self: *Lexer) !Token {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        while (self.current_idx < self.buf.len and std.ascii.isAlphanumeric(self.buf[self.current_idx]) or self.buf[self.current_idx] == '_') {
            try buf.append(self.buf[self.current_idx]);
            self.current_idx += 1;
            self.peek_idx += 1;
        }

        const ident: []u8 = try buf.toOwnedSlice();
        const const_ident: []const u8 = ident;

        return Token{ .type = TokenType.Identifier, .literal = const_ident };
    }

    fn chopWhiteSpace(self: *Lexer) void {
        while (self.current_idx < self.buf.len and self.buf[self.current_idx] == ' ') {
            self.current_idx += 1;
            self.peek_idx += 1;
        }
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const status = gpa.deinit();
        if (status != .ok) {
            std.debug.print("Memory leak detected: {}\n", .{status});
        }
    }

    const file = try std.fs.cwd().openFile("test.argos", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(content);

    var lexer = try Lexer.init(allocator, content);

    while (true) {
        const token = try lexer.next();
        std.debug.print("token: {any} --> {s}\n", .{token.type, token.literal});
        if (token.type == TokenType.Eof) {
            break;
        }
    }
}
