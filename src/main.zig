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
    Lt,
    Gt,

    Return,
    If,
    Else,

    NewLine,
    Eof,

    Plus,
    Minus,
    Slash,
    Asterisk,
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

// const ExpressionType = enum {
//     Identifier,
//     Prefix,
//     Infix,
//     Call,
//     Function,
//     String,
//     Number,
//     Boolean,
//     Array,
//     Table,
//     Index,
//     If,
//     Loop,
// };

const Expression = union(enum) {
    Identifier: []const u8,
    StringLiteral: []const u8,
    IntegerLiteral: i64,
    FloatLiteral: f64,
    BooleanLiteral: bool,
    InfixExpression: InfixExpression,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Identifier => |v| try writer.print("{s}", .{v}),
            .StringLiteral => |v| try writer.print("\"{s}\"", .{v}),
            .FloatLiteral => |v| try writer.print("{}", .{v}),
            .IntegerLiteral => |v| try writer.print("{}", .{v}),
            .BooleanLiteral => |v| try writer.print("{}", .{v}),
            .InfixExpression => |v| try writer.print("({any} {any} {any})", .{ v.left, v.operator, v.right }),
        }
    }
};

const Operator = enum {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Gt,
    Lt,
    Eq,
    NotEq,
};

const InfixExpression = struct {
    operator: Operator,
    left: *const Expression,
    right: *const Expression,
};

const Precedence = enum(u8) {
    Lowest = 1,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
};

const Parser = struct {
    cur_token: Token,
    peek_token: Token,
    lexer: *Lexer,
    allocator: std.mem.Allocator,

    fn init(lexer: *Lexer, allocator: std.mem.Allocator) !Parser {
        const cur_token = try lexer.next();
        if (cur_token.type == TokenType.Eof) {
            return error.NoTokens;
        }
        const peek_token = try lexer.next();
        return Parser{
            .lexer = lexer,
            .cur_token = cur_token,
            .peek_token = peek_token,
            .allocator = allocator,
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
            std.debug.print("expected: {any}, got: {any}\n", .{ expected_type, self.cur_token.type });
            return error.UnexpectedTokenType;
        }
    }

    fn advance_and_expect(self: *Parser, expected_type: TokenType) !void {
        try self.advance();
        if (self.cur_token.type != expected_type) {
            std.debug.print("expected: {any}, got: {any}\n", .{ expected_type, self.cur_token.type });
            return error.UnexpectedTokenType;
        }
    }

    fn expect_and_advance(self: *Parser, expected_type: TokenType) !void {
        if (self.cur_token.type != expected_type) {
            std.debug.print("expected: {any}, got: {any}\n", .{ expected_type, self.cur_token.type });
            return error.UnexpectedTokenType;
        }
        try self.advance();
    }

    fn parse_program(self: *Parser, allocator: std.mem.Allocator) !std.ArrayList(Statement) {
        var list = std.ArrayList(Statement).init(allocator);
        while (self.cur_token.type != TokenType.Eof) {
            const statement = try self.parse_statement();
            std.debug.print("Statement: {any}\n", .{statement});
            try list.append(statement);
        }
        return list;
    }

    fn parse_statement(self: *Parser) !Statement {
        return switch (self.cur_token.type) {
            TokenType.Identifier => {
                if (self.peek_token.type == TokenType.Assign) {
                    return try self.parse_assignment_statement();
                }
                return try self.parse_expression_statement();
            },
            TokenType.Return => try self.parse_return_statement(),
            else => try self.parse_expression_statement(),
        };
    }

    fn parse_expression_statement(self: *Parser) !Statement {
        const expression = try self.parse_expression(Precedence.Lowest);
        try self.advance_and_expect(TokenType.NewLine);
        try self.advance();
        return Statement{ .ExpressionStatement = .{ .expression = expression } };
    }

    fn parse_return_statement(self: *Parser) !Statement {
        try self.advance();
        const expression = try self.parse_expression(Precedence.Lowest);
        try self.advance_and_expect(TokenType.NewLine);
        try self.advance();
        return Statement{ .ReturnStatement = .{ .expression = expression } };
    }

    fn parse_assignment_statement(self: *Parser) !Statement {
        const identifier = try self.get_and_advance();
        try self.expect_and_advance(TokenType.Assign);
        const expression = try self.parse_expression(Precedence.Lowest);
        try self.advance_and_expect(TokenType.NewLine);
        try self.advance();
        return Statement{ .AssignmentStatement = AssignmentStatement{ .identifier = identifier, .expression = expression } };
    }

    fn get_prefix_expression(self: *Parser) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Identifier => Expression{ .Identifier = self.cur_token.literal },
            TokenType.String => Expression{ .StringLiteral = self.cur_token.literal },
            TokenType.Integer => Expression{ .IntegerLiteral = try std.fmt.parseInt(i64, self.cur_token.literal, 10) },
            TokenType.Float => Expression{ .FloatLiteral = try std.fmt.parseFloat(f64, self.cur_token.literal) },
            TokenType.False => Expression{ .BooleanLiteral = false },
            TokenType.True => Expression{ .BooleanLiteral = true },
            else => error.UnknownTokenType,
        };
    }

    fn get_infix_expression(self: *Parser, left: Expression) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Plus => self.parse_infix_expression(left),
            TokenType.Minus => self.parse_infix_expression(left),
            TokenType.Slash => self.parse_infix_expression(left),
            TokenType.Asterisk => self.parse_infix_expression(left),
            TokenType.Eq => self.parse_infix_expression(left),
            TokenType.NotEq => self.parse_infix_expression(left),
            TokenType.Lt => self.parse_infix_expression(left),
            TokenType.Gt => self.parse_infix_expression(left),
            else => error.NoInfixFunctionFound,
        };
    }

    fn parse_infix_expression(self: *Parser, left: Expression) !Expression {
        const precedence = self.get_current_precedence();
        const operator = try self.get_current_operator();
        const left_owned = try self.allocator.create(Expression);
        left_owned.* = left;

        std.debug.print("parse infix {any}\n", .{operator});

        try self.advance();

        const right = try self.parse_expression(precedence);
        const right_owned = try self.allocator.create(Expression);
        right_owned.* = right;

        const infix_expression = InfixExpression{
            .operator = operator,
            .left = left_owned,
            .right = right_owned,
        };

        return Expression{ .InfixExpression = infix_expression };
    }

    fn parse_expression(self: *Parser, precedence: Precedence) anyerror!Expression {
        var left: Expression = try self.get_prefix_expression();

        std.debug.print("{any}\n", .{self.cur_token});
        std.debug.print("{any}\n", .{self.peek_token.type});

        std.debug.print("{any} < {any}\n", .{ @intFromEnum(precedence), @intFromEnum(self.get_peek_precedence()) });

        while (self.peek_token.type != TokenType.NewLine and @intFromEnum(precedence) < @intFromEnum(self.get_peek_precedence())) {
            try self.advance();
            left = self.get_infix_expression(left) catch |err| {
                switch (err) {
                    error.NoInfixFunctionFound => return left,
                    else => return err,
                }
            };
        }

        std.debug.print("Returning {any}\n", .{left});

        return left;
    }

    fn get_current_precedence(self: *Parser) Precedence {
        return Parser.get_token_precedence(&self.cur_token);
    }

    fn get_peek_precedence(self: *Parser) Precedence {
        return Parser.get_token_precedence(&self.peek_token);
    }

    fn get_token_precedence(token: *Token) Precedence {
        return switch (token.type) {
            TokenType.Eq => Precedence.Equals,
            TokenType.NotEq => Precedence.Equals,
            TokenType.Lt => Precedence.Lessgreater,
            TokenType.Gt => Precedence.Lessgreater,
            TokenType.Plus => Precedence.Sum,
            TokenType.Minus => Precedence.Sum,
            TokenType.Slash => Precedence.Product,
            TokenType.Asterisk => Precedence.Product,
            TokenType.LParen => Precedence.Call,
            TokenType.LBracket => Precedence.Index,
            else => Precedence.Lowest,
        };
    }

    fn get_current_operator(self: *Parser) !Operator {
        return switch (self.cur_token.type) {
            TokenType.Plus => Operator.Plus,
            TokenType.Minus => Operator.Minus,
            TokenType.Asterisk => Operator.Asterisk,
            TokenType.Slash => Operator.Slash,
            TokenType.Gt => Operator.Gt,
            TokenType.Lt => Operator.Lt,
            TokenType.Eq => Operator.Eq,
            TokenType.NotEq => Operator.NotEq,
            else => {
                std.debug.print("unknown operator: {any}\n", .{self.cur_token.type});
                return error.NoOperatorFound;
            },
        };
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

    var parser = try Parser.init(&lexer, arenaAllocator);
    const program = try parser.parse_program(arenaAllocator);

    std.debug.print("{any}\n", .{program.items});
}
