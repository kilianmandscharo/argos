const std = @import("std");
const lexer_module = @import("lexer.zig");
const Token = lexer_module.Token;
const TokenType = lexer_module.TokenType;
const Lexer = lexer_module.Lexer;

pub const Statement = union(enum) {
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

pub const Expression = union(enum) {
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

pub const Operator = enum {
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

pub const Parser = struct {
    cur_token: Token,
    peek_token: Token,
    lexer: *Lexer,
    arena: std.mem.Allocator,

    pub fn init(lexer: *Lexer, arena: std.mem.Allocator) !Parser {
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

    pub fn parse_program(self: *Parser) !std.ArrayList(Statement) {
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
