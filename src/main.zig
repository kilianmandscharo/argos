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
    Function,

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

const Statement = union(enum) {
    AssignmentStatement: AssignmentStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,
    BlockStatement: BlockStatement,

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
    expression: *const Expression,
};

const ReturnStatement = struct {
    expression: *const Expression,
};

const ExpressionStatement = struct {
    expression: *const Expression,
};

const BlockStatement = struct {
    statements: std.ArrayList(Statement),
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
    PrefixExpression: PrefixExpression,
    FunctionLiteral: FunctionLiteral,

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
            .PrefixExpression => |v| try writer.print("({any}{any})", .{ v.operator, v.expression }),
            .FunctionLiteral => |v| {
                try writer.print("fnc(", .{});
                for (v.params.items, 0..) |param, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{any}", .{param});
                }
                try writer.print(") {{\n", .{});
                for (v.body.statements.items, 0..) |statement, i| {
                    if (i > 0) {
                        try writer.print("\n", .{});
                    }
                    try writer.print("    {any}", .{statement});
                }
                try writer.print("\n}}", .{});
            },
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
    Bang,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            Operator.Plus => try writer.print("+", .{}),
            Operator.Minus => try writer.print("-", .{}),
            Operator.Asterisk => try writer.print("*", .{}),
            Operator.Slash => try writer.print("/", .{}),
            Operator.Gt => try writer.print(">", .{}),
            Operator.Lt => try writer.print("<", .{}),
            Operator.Eq => try writer.print("==", .{}),
            Operator.NotEq => try writer.print("!=", .{}),
            Operator.Bang => try writer.print("!", .{}),
        }
    }
};

const InfixExpression = struct {
    operator: Operator,
    left: *const Expression,
    right: *const Expression,
};

const PrefixExpression = struct {
    operator: Operator,
    expression: *const Expression,
};

const FunctionLiteral = struct {
    params: std.ArrayList(*Expression),
    body: BlockStatement,
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
    arena: std.mem.Allocator,

    fn init(lexer: *Lexer, arena: std.mem.Allocator) !Parser {
        const cur_token = try lexer.next();
        if (cur_token.type == TokenType.Eof) {
            return error.NoTokens;
        }
        const peek_token = try lexer.next();
        return Parser{
            .lexer = lexer,
            .cur_token = cur_token,
            .peek_token = peek_token,
            .arena = arena,
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

    fn parse_program(self: *Parser) !std.ArrayList(Statement) {
        var list = std.ArrayList(Statement).init(self.arena);
        while (self.cur_token.type != TokenType.Eof) {
            const statement = try self.parse_statement();
            std.debug.print("{any}\n", .{statement});
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

    fn parse_prefix_expresion(self: *Parser) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Identifier => Expression{ .Identifier = self.cur_token.literal },
            TokenType.String => Expression{ .StringLiteral = self.cur_token.literal },
            TokenType.Integer => Expression{ .IntegerLiteral = try std.fmt.parseInt(i64, self.cur_token.literal, 10) },
            TokenType.Float => Expression{ .FloatLiteral = try std.fmt.parseFloat(f64, self.cur_token.literal) },
            TokenType.False => Expression{ .BooleanLiteral = false },
            TokenType.True => Expression{ .BooleanLiteral = true },
            TokenType.Bang, TokenType.Plus, TokenType.Minus => {
                const operator = try self.get_current_operator();
                try self.advance();
                const expression = try self.parse_expression(Precedence.Lowest);
                const prefix_expression = PrefixExpression{ .operator = operator, .expression = expression };
                return Expression{ .PrefixExpression = prefix_expression };
            },
            TokenType.Function => {
                try self.advance_and_expect(TokenType.LParen);
                try self.advance();

                var params = std.ArrayList(*Expression).init(self.arena);
                while (!self.current_token_is(TokenType.RParen)) {
                    if (self.current_token_is(TokenType.Eof)) {
                        return error.ReachedEndOfFile;
                    }

                    try params.append(try self.parse_expression(Precedence.Lowest));

                    if (!self.peek_token_is(TokenType.RParen)) {
                        try self.advance_and_expect(TokenType.Comma);
                        try self.advance();
                    } else {
                        try self.advance();
                    }
                }

                try self.advance_and_expect(TokenType.LBrace);
                try self.advance_and_expect(TokenType.NewLine);
                try self.advance();

                var statements = std.ArrayList(Statement).init(self.arena);
                while (!self.current_token_is(TokenType.RBrace)) {
                    if (self.current_token_is(TokenType.Eof)) {
                        return error.ReachedEndOfFile;
                    }

                    try statements.append(try self.parse_statement());
                }

                return Expression{ .FunctionLiteral = FunctionLiteral{ .params = params, .body = BlockStatement{ .statements = statements } } };
            },
            else => {
                std.debug.print("unknown token type: {any}\n", .{self.cur_token.type});
                return error.UnknownTokenType;
            },
        };
    }

    fn parse_infix_expression(self: *Parser, left: Expression) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Plus, TokenType.Minus, TokenType.Slash, TokenType.Asterisk, TokenType.Eq, TokenType.NotEq, TokenType.Lt, TokenType.Gt => {
                const precedence = self.get_current_precedence();
                const operator = try self.get_current_operator();

                const left_owned = try self.arena.create(Expression);
                left_owned.* = left;

                try self.advance();

                const right = try self.parse_expression(precedence);

                const infix_expression = InfixExpression{
                    .operator = operator,
                    .left = left_owned,
                    .right = right,
                };

                return Expression{ .InfixExpression = infix_expression };
            },
            else => error.NoInfixFunctionFound,
        };
    }

    fn parse_expression(self: *Parser, precedence: Precedence) anyerror!*Expression {
        var left = try self.parse_prefix_expresion();
        const left_owned = try self.arena.create(Expression);

        defer {
            left_owned.* = left;
        }

        while (self.peek_token.type != TokenType.NewLine and @intFromEnum(precedence) < @intFromEnum(self.get_peek_precedence())) {
            try self.advance();
            left = self.parse_infix_expression(left) catch |err| {
                switch (err) {
                    error.NoInfixFunctionFound => return left_owned,
                    else => return err,
                }
            };
        }

        return left_owned;
    }

    fn current_token_is(self: *Parser, token_type: TokenType) bool {
        return self.cur_token.type == token_type;
    }

    fn peek_token_is(self: *Parser, token_type: TokenType) bool {
        return self.peek_token.type == token_type;
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
            TokenType.Bang => Operator.Bang,
            else => {
                std.debug.print("unknown operator: {any}\n", .{self.cur_token.type});
                return error.NoOperatorFound;
            },
        };
    }
};

const Node = union(enum) {
    Program: std.ArrayList(Statement),
    Expression: Expression,
    Statement: Statement,
};

const Object = union(enum) {
    Integer: Integer,
    Float: Float,
    String: String,
    Boolean: Boolean,
    ReturnValue: ReturnValue,
    Error: Error,
    Null: Null,

    fn get_type(self: Object) []const u8 {
        return switch (self) {
            .Integer => "Integer",
            .Float => "Float",
            .String => "String",
            .Boolean => "Boolean",
            .ReturnValue => "ReturnValue",
            .Error => "Error",
            .Null => "Null",
        };
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Integer => |v| try writer.print("{d}", .{v.value}),
            .Float => |v| try writer.print("{d}", .{v.value}),
            .String => |v| try writer.print("{s}", .{v.value}),
            .Boolean => |v| try writer.print("{}", .{v.value}),
            .ReturnValue => |v| try writer.print("{any}", .{v.value}),
            .Error => |v| try writer.print("{s}", .{v.value}),
            .Null => try writer.print("Null", .{}),
        }
    }
};

const Integer = struct {
    value: i64,
};

const Float = struct {
    value: f64,
};

const String = struct {
    value: []const u8,
};

const Boolean = struct {
    value: bool,
};

const ReturnValue = struct {
    value: *const Object,
};

const Error = struct {
    value: []const u8,
};

const Null = struct {};

const Evaluator = struct {
    gpa: std.mem.Allocator,

    fn init(gpa: std.mem.Allocator) Evaluator {
        return Evaluator{
            .gpa = gpa,
        };
    }

    fn eval(self: *Evaluator, node: Node) Object {
        return switch (node) {
            .Program => |statements| {
                return self.eval_program(statements);
            },
            .Statement => |statement| {
                switch (statement) {
                    .ExpressionStatement => |expression_statement| {
                        return self.eval(Node{ .Expression = expression_statement.expression.* });
                    },
                    else => return Object{ .Error = .{ .value = "Unknown statement" } },
                }
            },
            .Expression => |expression| {
                switch (expression) {
                    .IntegerLiteral => |integer| return Object{ .Integer = .{ .value = integer } },
                    .FloatLiteral => |float| return Object{ .Float = .{ .value = float } },
                    .BooleanLiteral => |boolean| return Object{ .Boolean = .{ .value = boolean } },
                    .InfixExpression => |infix| {
                        const left = self.eval(Node{ .Expression = infix.left.* });
                        const right = self.eval(Node{ .Expression = infix.right.* });
                        return self.eval_infix_expression(left, right, infix.operator);
                    },
                    else => return Object{ .Error = .{ .value = "Unknown expression" } },
                }
            },
        };
    }

    fn eval_program(self: *Evaluator, statements: std.ArrayList(Statement)) Object {
        var result = Object{ .Null = .{} };
        for (statements.items) |statement| {
            result = self.eval(Node{ .Statement = statement });
            switch (result) {
                .ReturnValue => |retVal| return retVal.value.*,
                .Error => return result,
                else => {},
            }
        }
        return result;
    }

    fn eval_infix_expression(self: *Evaluator, left: Object, right: Object, operator: Operator) Object {
        switch (left) {
            .Integer => |left_int| {
                switch (right) {
                    .Integer => |right_int| {
                        return self.eval_integer_infix_expression(left_int.value, right_int.value, operator);
                    },
                    .Float => |right_float| {
                        return self.eval_float_infix_expression(@floatFromInt(left_int.value), right_float.value, operator);
                    },
                    else => return self.create_error("type mismatch: {s} <> {s}", .{ left.get_type(), right.get_type() }),
                }
            },
            .Float => |left_float| {
                switch (right) {
                    .Integer => |right_int| {
                        return self.eval_float_infix_expression(left_float.value, @floatFromInt(right_int.value), operator);
                    },
                    .Float => |right_float| {
                        return self.eval_float_infix_expression(left_float.value, right_float.value, operator);
                    },
                    else => return self.create_error("type mismatch: {s} <> {s}", .{ left.get_type(), right.get_type() }),
                }
            },
            .Boolean => |left_bool| {
                switch (right) {
                    .Boolean => |right_bool| {
                        return self.eval_boolean_infix_expression(left_bool.value, right_bool.value, operator);
                    },
                    else => return self.create_error("type mismatch: {s} <> {s}", .{ left.get_type(), right.get_type() }),
                }
            },
            else => return Object{ .Error = .{ .value = "Unknown left expression type" } },
        }
    }

    fn eval_boolean_infix_expression(self: *Evaluator, left: bool, right: bool, operator: Operator) Object {
        return switch (operator) {
            .Eq => Object{ .Boolean = .{ .value = left == right } },
            .NotEq => Object{ .Boolean = .{ .value = left != right } },
            else => self.create_error("invalid operator '{any}' for type Boolean", .{operator}),
        };
    }

    fn eval_float_infix_expression(self: *Evaluator, left: f64, right: f64, operator: Operator) Object {
        return switch (operator) {
            .Plus => Object{ .Float = .{ .value = left + right } },
            .Minus => Object{ .Float = .{ .value = left - right } },
            .Slash => Object{ .Float = .{ .value = left / right } },
            .Asterisk => Object{ .Float = .{ .value = left * right } },
            .Gt => Object{ .Boolean = .{ .value = left > right } },
            .Lt => Object{ .Boolean = .{ .value = left < right } },
            .Eq => Object{ .Boolean = .{ .value = left == right } },
            .NotEq => Object{ .Boolean = .{ .value = left != right } },
            else => self.create_error("invalid operator '{any}' for type Float", .{operator}),
        };
    }

    fn eval_integer_infix_expression(self: *Evaluator, left: i64, right: i64, operator: Operator) Object {
        return switch (operator) {
            .Plus => Object{ .Integer = .{ .value = left + right } },
            .Minus => Object{ .Integer = .{ .value = left - right } },
            .Slash => {
                const left_float: f64 = @floatFromInt(left);
                const right_float: f64 = @floatFromInt(right);
                return Object{ .Float = .{ .value = left_float / right_float } };
            },
            .Asterisk => Object{ .Integer = .{ .value = left * right } },
            .Gt => Object{ .Boolean = .{ .value = left > right } },
            .Lt => Object{ .Boolean = .{ .value = left < right } },
            .Eq => Object{ .Boolean = .{ .value = left == right } },
            .NotEq => Object{ .Boolean = .{ .value = left != right } },
            else => self.create_error("invalid operator '{any}' for type Integer", .{operator}),
        };
    }

    fn create_error(self: *Evaluator, comptime fmt: []const u8, args: anytype) Object {
        const error_message = std.fmt.allocPrint(self.gpa, fmt, args) catch |err| {
            std.debug.print("failed to alloc error message: {}", .{err});
            return Object{ .Error = .{ .value = "Unknown error" } };
        };
        return Object{ .Error = .{ .value = error_message } };
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
    const program = try parser.parse_program();

    var evaluator = Evaluator.init(gpaAllocator);
    const result = evaluator.eval(Node{ .Program = program });
    std.debug.print("Result: {any}\n", .{result});
}
