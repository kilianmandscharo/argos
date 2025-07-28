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
        writer: anytype,
    ) !void {
        switch (self) {
            .AssignmentStatement => |v| try writer.print("{s} = {f}", .{ v.identifier.literal, v.expression }),
            .ReturnStatement => |v| try writer.print("return {f}", .{v.expression}),
            .ExpressionStatement => |v| try writer.print("{f}", .{v.expression}),
            .BlockStatement => |v| try v.format(writer),
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

pub const BlockStatement = struct {
    statements: std.ArrayListUnmanaged(Statement),
    is_one_liner: bool,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        var separator: []const u8 = if (self.is_one_liner) " " else "\n";
        if (self.statements.items.len == 0) separator = "";
        const ident = if (self.is_one_liner) "" else "    ";
        try writer.print("{{{s}", .{separator});
        for (self.statements.items, 0..) |statement, i| {
            try writer.print("{s}{f}", .{ ident, statement });
            if (!self.is_one_liner and i < self.statements.items.len - 1) {
                try writer.print("\n", .{});
            }
        }
        try writer.print("{s}}}", .{separator});
    }
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
    CallExpression: CallExpression,
    IfExpression: IfExpression,
    RangeExpression: RangeExpression,
    ForExpression: ForExpression,

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        switch (self) {
            .Identifier => |v| try writer.print("{s}", .{v}),
            .StringLiteral => |v| try writer.print("\"{s}\"", .{v}),
            .FloatLiteral => |v| try writer.print("{}", .{v}),
            .IntegerLiteral => |v| try writer.print("{}", .{v}),
            .BooleanLiteral => |v| try writer.print("{}", .{v}),
            .InfixExpression => |v| try writer.print("({f} {f} {f})", .{ v.left, v.operator, v.right }),
            .PrefixExpression => |v| try writer.print("({f}{f})", .{ v.operator, v.expression }),
            .FunctionLiteral => |v| {
                try writer.print("fnc {s}(", .{v.name});
                for (v.params.items, 0..) |param, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{s}", .{param});
                }
                try writer.print(") ", .{});
                try v.body.format(writer);
            },
            .CallExpression => |v| {
                try writer.print("({f}(", .{v.function});
                for (v.args.items, 0..) |arg, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{f}", .{arg});
                }
                try writer.print("))", .{});
            },
            .IfExpression => |v| {
                try writer.print("if {f} ", .{v.condition});
                try v.body.format(writer);
                if (v.alternative) |alternative| {
                    try writer.print(" else ", .{});
                    try alternative.format(writer);
                }
            },
            .RangeExpression => |v| {
                try writer.print("({f}..{f})", .{ v.left, v.right });
            },
            .ForExpression => |v| {
                try writer.print("for {s} in ({f}..{f}) ", .{ v.variable, v.range.left, v.range.right });
                try v.body.format(writer);
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
        writer: anytype,
    ) !void {
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
    name: []const u8,
    params: std.ArrayListUnmanaged([]const u8),
    body: BlockStatement,
    is_one_liner: bool,
};

const CallExpression = struct {
    function: *const Expression,
    args: std.ArrayList(*Expression),
};

const IfExpression = struct {
    condition: *const Expression,
    body: BlockStatement,
    alternative: ?BlockStatement,
    is_one_liner: bool,
};

const RangeExpression = struct {
    left: *const Expression,
    right: *const Expression,
};

const ForExpression = struct {
    variable: []const u8,
    range: RangeExpression,
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

pub const ParserError = error{
    UnexpectedTokenType,
    NoInfixFunctionFound,
    ReachedEndOfFile,
    UnknownTokenType,
    NoTokens,
    NoOperatorFound,
};

pub const Parser = struct {
    cur_token: Token,
    peek_token: Token,
    lexer: *Lexer,
    arena: std.mem.Allocator,

    pub fn init(lexer: *Lexer, arena: std.mem.Allocator) !Parser {
        const cur_token = try lexer.next();
        if (cur_token.type == TokenType.Eof) {
            return ParserError.NoTokens;
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
        std.debug.print("cur token: {f}\n", .{self.cur_token});
    }

    fn getAndAdvance(self: *Parser) !Token {
        const cur_token = self.cur_token;
        try self.advance();
        return cur_token;
    }

    fn expectToken(self: *Parser, expected_type: TokenType) !void {
        if (self.cur_token.type != expected_type) {
            return ParserError.UnexpectedTokenType;
        }
    }

    fn advanceAndExpect(self: *Parser, expected_type: TokenType) !void {
        try self.advance();
        if (self.cur_token.type != expected_type) {
            std.debug.print("expected: {any}, got: {any}\n", .{ expected_type, self.cur_token.type });
            return ParserError.UnexpectedTokenType;
        }
    }

    fn expectAndAdvance(self: *Parser, expected_type: TokenType) !void {
        if (self.cur_token.type != expected_type) {
            std.debug.print("expected: {any}, got: {any}\n", .{ expected_type, self.cur_token.type });
            return ParserError.UnexpectedTokenType;
        }
        try self.advance();
    }

    pub fn parseProgram(self: *Parser) !std.ArrayList(Statement) {
        var list = std.ArrayList(Statement).init(self.arena);
        while (self.cur_token.type != TokenType.Eof) {
            const statement = try self.parseStatement();
            try list.append(statement);
            while (!self.currentTokenIs(.Eof)) {
                if (self.currentTokenIs(.NewLine)) {
                    try self.advance();
                } else {
                    break;
                }
            }
        }
        return list;
    }

    pub fn printProgram(program: std.ArrayList(Statement), arena: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(arena);
        for (program.items, 0..) |statement, i| {
            try std.fmt.format(buffer.writer(), "{f}", .{statement});
            if (i < program.items.len - 1) {
                try buffer.append('\n');
            }
        }
        return buffer.items;
    }

    fn parseStatement(self: *Parser) !Statement {
        return switch (self.cur_token.type) {
            .Identifier => {
                if (self.peek_token.type == .Assign) {
                    return try self.parseAssignmentStatement();
                }
                return try self.parseExpressionStatement();
            },
            .Return => try self.parseReturnStatement(),
            else => try self.parseExpressionStatement(),
        };
    }

    fn parseExpressionStatement(self: *Parser) !Statement {
        const expression = try self.parseExpression(.Lowest);
        const statement = Statement{ .ExpressionStatement = .{ .expression = expression } };
        try self.advance();
        return statement;
    }

    fn parseReturnStatement(self: *Parser) !Statement {
        try self.advance();
        const expression = try self.parseExpression(Precedence.Lowest);
        const statement = Statement{ .ReturnStatement = .{ .expression = expression } };
        try self.advance();
        return statement;
    }

    fn parseAssignmentStatement(self: *Parser) !Statement {
        const identifier = try self.getAndAdvance();
        try self.expectAndAdvance(TokenType.Assign);
        const expression = try self.parseExpression(Precedence.Lowest);
        const statement = Statement{ .AssignmentStatement = AssignmentStatement{ .identifier = identifier, .expression = expression } };
        try self.advance();
        return statement;
    }

    fn parseBlockStatement(self: *Parser, is_one_liner: bool) !BlockStatement {
        var statements: std.ArrayListUnmanaged(Statement) = .{};
        while (!self.currentTokenIs(TokenType.RBrace)) {
            if (self.currentTokenIs(TokenType.Eof)) {
                return ParserError.ReachedEndOfFile;
            }

            if (self.currentTokenIs(.NewLine)) {
                try self.advance();
                continue;
            }

            try statements.append(self.arena, try self.parseStatement());

            if (!is_one_liner) {
                try self.expectAndAdvance(.NewLine);
            } else if (self.currentTokenIs(.NewLine)) {
                return ParserError.UnexpectedTokenType;
            }
        }
        return BlockStatement{
            .statements = statements,
            .is_one_liner = is_one_liner,
        };
    }

    fn parsePrefixExpression(self: *Parser) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Identifier => Expression{ .Identifier = self.cur_token.literal },
            TokenType.String => Expression{ .StringLiteral = self.cur_token.literal },
            TokenType.Integer => Expression{ .IntegerLiteral = try std.fmt.parseInt(i64, self.cur_token.literal, 10) },
            TokenType.Float => Expression{ .FloatLiteral = try std.fmt.parseFloat(f64, self.cur_token.literal) },
            TokenType.False => Expression{ .BooleanLiteral = false },
            TokenType.True => Expression{ .BooleanLiteral = true },
            TokenType.Bang, TokenType.Plus, TokenType.Minus => {
                const operator = try self.getCurrentOperator();
                try self.advance();
                const expression = try self.parseExpression(Precedence.Lowest);
                const prefix_expression = PrefixExpression{ .operator = operator, .expression = expression };
                return Expression{ .PrefixExpression = prefix_expression };
            },
            TokenType.Function => {
                var name: []const u8 = "";
                if (self.peekTokenIs(.Identifier)) {
                    try self.advance();
                    name = self.cur_token.literal;
                }

                try self.advanceAndExpect(TokenType.LParen);
                try self.advance();

                var params: std.ArrayListUnmanaged([]const u8) = .{};
                while (!self.currentTokenIs(TokenType.RParen)) {
                    if (self.currentTokenIs(TokenType.Eof)) {
                        return ParserError.ReachedEndOfFile;
                    }

                    try self.expectToken(TokenType.Identifier);
                    try params.append(self.arena, self.cur_token.literal);

                    if (!self.peekTokenIs(TokenType.RParen)) {
                        try self.advanceAndExpect(TokenType.Comma);
                    }

                    try self.advance();
                }

                var is_one_liner = true;

                try self.advanceAndExpect(TokenType.LBrace);
                if (self.peekTokenIs(.NewLine)) {
                    is_one_liner = false;
                    try self.advance();
                }
                try self.advance();

                const body = try self.parseBlockStatement(is_one_liner);

                return Expression{
                    .FunctionLiteral = FunctionLiteral{ .name = name, .params = params, .body = body, .is_one_liner = is_one_liner },
                };
            },
            .If => {
                try self.advance();

                const condition = try self.parseExpression(.Lowest);

                var is_one_liner = true;

                try self.advanceAndExpect(TokenType.LBrace);
                if (self.peekTokenIs(.NewLine)) {
                    is_one_liner = false;
                    try self.advance();
                }
                try self.advance();

                const body = try self.parseBlockStatement(is_one_liner);

                var if_expression = Expression{ .IfExpression = .{ .condition = condition, .body = body, .alternative = null, .is_one_liner = is_one_liner } };

                try self.advance();

                if (!self.currentTokenIs(.Else)) {
                    if (!self.currentTokenIs(.Eof)) {
                        try self.expectAndAdvance(.NewLine);
                    }
                    return if_expression;
                }

                try self.advanceAndExpect(.LBrace);
                try self.advance();

                if_expression.IfExpression.alternative = try self.parseBlockStatement(is_one_liner);
                return if_expression;
            },
            .For => {
                try self.advance();

                const variable = try self.parseExpression(.Lowest);
                if (variable.* != .Identifier) {
                    return error.ExpectedIdentifier;
                }

                try self.advanceAndExpect(.In);
                try self.advance();

                const range = try self.parseExpression(.Lowest);
                if (range.* != .RangeExpression) {
                    return error.ExpectedRange;
                }

                try self.advance();
                try self.expectAndAdvance(.LBrace);
                const body = try self.parseBlockStatement(false);

                return Expression{
                    .ForExpression = ForExpression{
                        .variable = variable.Identifier,
                        .range = range.RangeExpression,
                        .body = body,
                    },
                };
            },
            else => {
                std.debug.print("unknown token type: {any}\n", .{self.cur_token.type});
                return ParserError.UnknownTokenType;
            },
        };
    }

    fn parseInfixExpression(self: *Parser, left: Expression) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Plus, TokenType.Minus, TokenType.Slash, TokenType.Asterisk, TokenType.Eq, TokenType.NotEq, TokenType.Lt, TokenType.Gt => {
                const precedence = self.getCurrentPrecedence();
                const operator = try self.getCurrentOperator();

                const left_owned = try self.arena.create(Expression);
                left_owned.* = left;

                try self.advance();

                const right = try self.parseExpression(precedence);

                const infix_expression = InfixExpression{
                    .operator = operator,
                    .left = left_owned,
                    .right = right,
                };

                return Expression{ .InfixExpression = infix_expression };
            },
            TokenType.LParen => {
                try self.advance();

                var args = std.ArrayList(*Expression).init(self.arena);
                while (!self.currentTokenIs(TokenType.RParen)) {
                    if (self.currentTokenIs(TokenType.Eof)) {
                        return ParserError.ReachedEndOfFile;
                    }

                    try args.append(try self.parseExpression(Precedence.Lowest));

                    if (!self.peekTokenIs(TokenType.RParen)) {
                        try self.advanceAndExpect(TokenType.Comma);
                    }

                    try self.advance();
                }

                const left_owned = try self.arena.create(Expression);
                left_owned.* = left;

                return Expression{ .CallExpression = .{ .function = left_owned, .args = args } };
            },
            TokenType.DotDot => {
                try self.advance();
                const right = try self.parseExpression(.Lowest);
                const left_owned = try self.arena.create(Expression);
                left_owned.* = left;
                return Expression{ .RangeExpression = .{ .left = left_owned, .right = right } };
            },
            else => ParserError.NoInfixFunctionFound,
        };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) anyerror!*Expression {
        var left = try self.parsePrefixExpression();
        const left_owned = try self.arena.create(Expression);

        defer {
            left_owned.* = left;
        }

        while (self.peek_token.type != TokenType.NewLine and @intFromEnum(precedence) < @intFromEnum(self.getPeekPrecedence())) {
            try self.advance();
            left = self.parseInfixExpression(left) catch |err| {
                switch (err) {
                    ParserError.NoInfixFunctionFound => return left_owned,
                    else => return err,
                }
            };
        }

        return left_owned;
    }

    fn currentTokenIs(self: *Parser, token_type: TokenType) bool {
        return self.cur_token.type == token_type;
    }

    fn peekTokenIs(self: *Parser, token_type: TokenType) bool {
        return self.peek_token.type == token_type;
    }

    fn getCurrentPrecedence(self: *Parser) Precedence {
        return Parser.getTokenPrecedence(&self.cur_token);
    }

    fn getPeekPrecedence(self: *Parser) Precedence {
        return Parser.getTokenPrecedence(&self.peek_token);
    }

    fn getTokenPrecedence(token: *Token) Precedence {
        return switch (token.type) {
            TokenType.DotDot => Precedence.Equals,
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

    fn getCurrentOperator(self: *Parser) !Operator {
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
                return ParserError.NoOperatorFound;
            },
        };
    }
};
