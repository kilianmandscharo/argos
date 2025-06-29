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
    True,
    False,

    Eq,
    NotEq,
    Bang,
    Identifier,
    Assign,
    Comma,

    Return,
    If,
    Else,

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

const Lexer = struct {
    buf: []const u8,
    pos: usize,
    arena: std.mem.Allocator,

    fn init(arena: std.mem.Allocator, content: []const u8) !Lexer {
        if (content.len == 0) {
            return error.NoContent;
        }
        return Lexer{
            .buf = content,
            .pos = 0,
            .arena = arena,
        };
    }

    fn next(self: *Lexer) !Token {
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

const Statement = union(enum) {
    AssignmentStatement: AssignmentStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,
    BlockStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .AssignmentStatement => |v| try writer.print("{s} = {any}", .{ v.identifier.literal, v.expression }),
            .ReturnStatement => |v| try writer.print("return {any}", .{v.expression}),
            .ExpressionStatement => |v| try writer.print("{any}", .{v.expression}),
            else => try writer.print("no custom formatter yet", .{}),
        }
    }
};

const AssignmentStatement = struct {
    identifier: Token,
    expression: Expression,
};

const ReturnStatement = struct {
    expression: Expression,
};

const ExpressionStatement = struct {
    expression: Expression,
};

const ExpressionType = enum {
    Identifier,
    Prefix,
    Infix,
    Call,
    Function,
    String,
    Number,
    Boolean,
    Array,
    Table,
    Index,
    If,
    Loop,
};

const Expression = union(enum) {
    Identifier: Identifier,
    StringLiteral: StringLiteral,
    NumberLiteral: NumberLiteral,
    BooleanLiteral: BooleanLiteral,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Identifier => |v| try writer.print("{s}", .{v.token.literal}),
            .StringLiteral => |v| try writer.print("\"{s}\"", .{v.token.literal}),
            .NumberLiteral => |v| try writer.print("{s}", .{v.token.literal}),
            .BooleanLiteral => |v| try writer.print("{s}", .{v.token.literal}),
        }
    }
};

const StringLiteral = struct {
    token: Token,
};

const NumberLiteral = struct {
    token: Token,
};

const BooleanLiteral = struct {
    token: Token,
};

const Identifier = struct {
    token: Token,
};

const Parser = struct {
    cur_token: Token,
    peek_token: Token,
    lexer: *Lexer,

    fn init(lexer: *Lexer) !Parser {
        const cur_token = try lexer.next();
        if (cur_token.type == TokenType.Eof) {
            return error.NoTokens;
        }
        const peek_token = try lexer.next();
        return Parser{
            .lexer = lexer,
            .cur_token = cur_token,
            .peek_token = peek_token,
        };
    }

    fn advance(self: *Parser) !void {
        self.cur_token = self.peek_token;
        self.peek_token = try self.lexer.next();
    }

    fn get_and_advance(self: *Parser) !Token {
        const cur_token = self.cur_token;
        try self.advance();
        return cur_token;
    }

    fn expect_token(self: *Parser, expected_type: TokenType) !void {
        if (self.cur_token.type != expected_type) {
            return error.UnexpectedTokenType;
        }
    }

    fn advance_and_expect(self: *Parser, expected_type: TokenType) !void {
        try self.advance();
        if (self.cur_token.type != expected_type) {
            return error.UnexpectedTokenType;
        }
    }

    fn expect_and_advance(self: *Parser, expected_type: TokenType) !void {
        if (self.cur_token.type != expected_type) {
            return error.UnexpectedTokenType;
        }
        try self.advance();
    }

    fn parse_program(self: *Parser, allocator: std.mem.Allocator) !std.ArrayList(Statement) {
        var list = std.ArrayList(Statement).init(allocator);
        while (self.cur_token.type != TokenType.Eof) {
            try list.append(try self.parse_statement());
        }
        return list;
    }

    fn parse_statement(self: *Parser) !Statement {
        const statement = switch (self.cur_token.type) {
            TokenType.Identifier => try self.parse_assignment_statement(),
            TokenType.Return => try self.parse_return_statement(),
            else => try self.parse_expression_statement(),
        };
        std.debug.print("{any}\n", .{statement});
        return statement;
    }

    fn parse_expression_statement(self: *Parser) !Statement {
        const expression = try self.parse_expression();
        try self.expect_and_advance(TokenType.NewLine);
        return Statement{ .ExpressionStatement = .{ .expression = expression } };
    }

    fn parse_return_statement(self: *Parser) !Statement {
        try self.advance();
        const expression = try self.parse_expression();
        try self.expect_and_advance(TokenType.NewLine);
        return Statement{ .ReturnStatement = .{ .expression = expression } };
    }

    fn parse_assignment_statement(self: *Parser) !Statement {
        const identifier = try self.get_and_advance();
        try self.expect_and_advance(TokenType.Assign);
        const expression = try self.parse_expression();
        try self.expect_and_advance(TokenType.NewLine);
        return Statement{ .AssignmentStatement = AssignmentStatement{ .identifier = identifier, .expression = expression } };
    }

    fn get_prefix_expression(self: *Parser) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Identifier => Expression{ .Identifier = Identifier{ .token = self.cur_token } },
            TokenType.String => Expression{ .StringLiteral = StringLiteral{ .token = self.cur_token } },
            TokenType.Integer => Expression{ .NumberLiteral = NumberLiteral{ .token = self.cur_token } },
            TokenType.Float => Expression{ .NumberLiteral = NumberLiteral{ .token = self.cur_token } },
            TokenType.False => Expression{ .BooleanLiteral = BooleanLiteral{ .token = self.cur_token } },
            TokenType.True => Expression{ .BooleanLiteral = BooleanLiteral{ .token = self.cur_token } },
            else => error.UnknownTokenType,
        };
    }

    fn parse_expression(self: *Parser) !Expression {
        const expression = try self.get_prefix_expression();
        try self.advance();
        return expression;
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

    var lexer = try Lexer.init(arenaAllocator, content);

    // while (true) {
    //     const token = try lexer.next();
    //     std.debug.print("{any} --> {s}\n", .{ token.type, token.literal });
    //     if (token.type == TokenType.Eof) {
    //         break;
    //     }
    // }

    var parser = try Parser.init(&lexer);
    const program = try parser.parse_program(arenaAllocator);

    std.debug.print("{any}\n", .{program.items});
}
