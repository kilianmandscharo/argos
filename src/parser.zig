const std = @import("std");

const lexer_module = @import("lexer.zig");
const Token = lexer_module.Token;
const TokenType = lexer_module.TokenType;
const Lexer = lexer_module.Lexer;

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
    ArrayLiteral: std.ArrayListUnmanaged(*const Expression),
    TableLiteral: std.ArrayListUnmanaged(*const Expression),
    BlockExpression: BlockExpression,
    AssignmentExpression: AssignmentExpression,
    ReturnExpression: *const Expression,
    IndexExpression: IndexExpression,

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
            .ArrayLiteral => {
                try writer.print("array format not implemented", .{});
            },
            .TableLiteral => {
                try writer.print("table format not implemented", .{});
            },
            .IndexExpression => {
                try writer.print("index expression format not implemented", .{});
            },
            .BlockExpression => |v| try v.format(writer),
            .AssignmentExpression => |v| try writer.print("({s} = {f})", .{ v.identifier, v.expression }),
            .ReturnExpression => |v| try writer.print("(return {f})", .{v}),
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
    Assign,

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
            Operator.Assign => try writer.print("=", .{}),
        }
    }
};

pub const InfixExpression = struct {
    operator: Operator,
    left: *const Expression,
    right: *const Expression,
};

pub const PrefixExpression = struct {
    operator: Operator,
    expression: *const Expression,
};

pub const FunctionLiteral = struct {
    name: []const u8,
    params: std.ArrayListUnmanaged([]const u8),
    body: BlockExpression,
    is_one_liner: bool,
};

pub const CallExpression = struct {
    function: *const Expression,
    args: std.ArrayListUnmanaged(*const Expression),
};

pub const IfExpression = struct {
    condition: *const Expression,
    body: BlockExpression,
    alternative: ?BlockExpression,
    is_one_liner: bool,
};

pub const RangeExpression = struct {
    left: *const Expression,
    right: *const Expression,
};

pub const ForExpression = struct {
    variable: []const u8,
    range: RangeExpression,
    body: BlockExpression,
};

pub const BlockExpression = struct {
    expressions: std.ArrayListUnmanaged(*const Expression),
    is_one_liner: bool,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        var separator: []const u8 = if (self.is_one_liner) " " else "\n";
        if (self.expressions.items.len == 0) separator = "";
        const ident = if (self.is_one_liner) "" else "    ";
        try writer.print("{{{s}", .{separator});
        for (self.expressions.items, 0..) |expression, i| {
            try writer.print("{s}{f}", .{ ident, expression });
            if (!self.is_one_liner and i < self.expressions.items.len - 1) {
                try writer.print("\n", .{});
            }
        }
        try writer.print("{s}}}", .{separator});
    }
};

pub const AssignmentExpression = struct {
    identifier: []const u8,
    expression: *const Expression,
};

pub const IndexExpression = struct {
    left: *const Expression,
    index_expression: *const Expression,
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

    pub fn parseProgram(self: *Parser) !std.ArrayListUnmanaged(*const Expression) {
        var list: std.ArrayListUnmanaged(*const Expression) = .{};
        while (self.cur_token.type != TokenType.Eof) {
            const expression = try self.parseExpression(.Lowest);
            try self.advance();
            try list.append(self.arena, expression);
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

    fn parseExpression(self: *Parser, precedence: Precedence) anyerror!*Expression {
        var left = try self.parseLeft();
        const left_owned = try self.arena.create(Expression);

        defer {
            left_owned.* = left;
        }

        while (self.peek_token.type != TokenType.NewLine and @intFromEnum(precedence) < @intFromEnum(self.getPeekPrecedence())) {
            try self.advance();
            left = self.parseRight(left) catch |err| {
                switch (err) {
                    ParserError.NoInfixFunctionFound => return left_owned,
                    else => return err,
                }
            };
        }

        return left_owned;
    }

    fn parseLeft(self: *Parser) !Expression {
        return switch (self.cur_token.type) {
            .Identifier => self.parseIdentifier(),
            .String => self.parseString(),
            .Integer => try self.parseInteger(),
            .Float => try self.parseFloat(),
            .True, .False => try self.parseBoolean(),
            .Bang, .Plus, .Minus => try self.parsePrefix(),
            .Function => try self.parseFunction(),
            .If => try self.parseIf(),
            .For => try self.parseFor(),
            .LBracket => try self.parseBracket(),
            .LBrace => try self.parseBrace(),
            .Return => try self.parseReturn(),
            else => {
                std.debug.print("unknown token type: {any}\n", .{self.cur_token.type});
                return ParserError.UnknownTokenType;
            },
        };
    }

    fn parseIdentifier(self: *Parser) Expression {
        return Expression{ .Identifier = self.cur_token.literal };
    }

    fn parseString(self: *Parser) Expression {
        return Expression{ .StringLiteral = self.cur_token.literal };
    }

    fn parseInteger(self: *Parser) !Expression {
        return Expression{ .IntegerLiteral = try std.fmt.parseInt(i64, self.cur_token.literal, 10) };
    }

    fn parseFloat(self: *Parser) !Expression {
        return Expression{ .FloatLiteral = try std.fmt.parseFloat(f64, self.cur_token.literal) };
    }

    fn parseBoolean(self: *Parser) !Expression {
        return switch (self.cur_token.type) {
            .True => Expression{ .BooleanLiteral = true },
            .False => Expression{ .BooleanLiteral = false },
            else => error.InvalidBooleanLiteral,
        };
    }

    fn parsePrefix(self: *Parser) !Expression {
        const operator = try self.getCurrentOperator();
        try self.advance();
        const expression = try self.parseExpression(Precedence.Lowest);
        const prefix_expression = PrefixExpression{ .operator = operator, .expression = expression };
        return Expression{ .PrefixExpression = prefix_expression };
    }

    fn parseFunction(self: *Parser) !Expression {
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
        }

        const result = try self.parseExpressionList();

        return Expression{
            .FunctionLiteral = FunctionLiteral{
                .name = name,
                .params = params,
                .body = BlockExpression{
                    .expressions = result.items,
                    .is_one_liner = result.is_one_liner,
                },
                .is_one_liner = is_one_liner,
            },
        };
    }

    fn parseIf(self: *Parser) !Expression {
        try self.advance();

        const condition = try self.parseExpression(.Lowest);

        var is_one_liner = true;

        try self.advanceAndExpect(TokenType.LBrace);
        if (self.peekTokenIs(.NewLine)) {
            is_one_liner = false;
        }

        const result = try self.parseExpressionList();

        var if_expression = Expression{
            .IfExpression = .{
                .condition = condition,
                .body = BlockExpression{
                    .expressions = result.items,
                    .is_one_liner = result.is_one_liner,
                },
                .alternative = null,
                .is_one_liner = is_one_liner,
            },
        };

        try self.advance();

        if (!self.currentTokenIs(.Else)) {
            if (!self.currentTokenIs(.Eof)) {
                try self.expectAndAdvance(.NewLine);
            }
            return if_expression;
        }

        try self.advanceAndExpect(.LBrace);

        const alternative = try self.parseExpressionList();
        if_expression.IfExpression.alternative = BlockExpression{
            .expressions = alternative.items,
            .is_one_liner = alternative.is_one_liner,
        };
        return if_expression;
    }

    fn parseFor(self: *Parser) !Expression {
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

        try self.advanceAndExpect(.LBrace);

        const body = try self.parseExpressionList();

        return Expression{
            .ForExpression = ForExpression{
                .variable = variable.Identifier,
                .range = range.RangeExpression,
                .body = BlockExpression{
                    .expressions = body.items,
                    .is_one_liner = body.is_one_liner,
                },
            },
        };
    }

    fn parseBracket(self: *Parser) !Expression {
        const result = try self.parseExpressionList();
        return Expression{
            .ArrayLiteral = result.items,
        };
    }

    fn parseBrace(self: *Parser) !Expression {
        const expression_list = try self.parseExpressionList();

        const expression = switch (expression_list.delimiter) {
            .NewLine => Expression{
                .BlockExpression = BlockExpression{
                    .expressions = expression_list.items,
                    .is_one_liner = expression_list.is_one_liner,
                },
            },
            .Comma => Expression{
                .TableLiteral = expression_list.items,
            },
            else => return error.UnknownExpressionListDelimiter,
        };

        return expression;
    }

    fn parseReturn(self: *Parser) !Expression {
        try self.advance();
        const expression = try self.parseExpression(.Lowest);
        return Expression{ .ReturnExpression = expression };
    }

    fn parseRight(self: *Parser, left: Expression) !Expression {
        return switch (self.cur_token.type) {
            TokenType.Plus, TokenType.Minus, TokenType.Slash, TokenType.Asterisk, TokenType.Eq, TokenType.NotEq, TokenType.Lt, TokenType.Gt => self.parseInfix(left),
            TokenType.LParen => self.parseCall(left),
            TokenType.DotDot => self.parseRange(left),
            TokenType.Assign => self.parseAssign(left),
            TokenType.LBracket => self.parseIndex(left),
            else => ParserError.NoInfixFunctionFound,
        };
    }

    fn parseInfix(self: *Parser, left: Expression) !Expression {
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
    }

    fn parseCall(self: *Parser, left: Expression) !Expression {
        const result = try self.parseExpressionList();
        const left_owned = try self.arena.create(Expression);
        left_owned.* = left;
        return Expression{ .CallExpression = .{ .function = left_owned, .args = result.items } };
    }

    fn parseRange(self: *Parser, left: Expression) !Expression {
        try self.advance();
        const right = try self.parseExpression(.Lowest);
        const left_owned = try self.arena.create(Expression);
        left_owned.* = left;
        return Expression{ .RangeExpression = .{ .left = left_owned, .right = right } };
    }

    fn parseAssign(self: *Parser, left: Expression) !Expression {
        try self.advance();
        const expression = try self.parseExpression(.Lowest);
        if (left == .Identifier) {
            return Expression{
                .AssignmentExpression = AssignmentExpression{
                    .identifier = left.Identifier,
                    .expression = expression,
                },
            };
        }
        return error.ExpectedIdentifier;
    }

    fn parseIndex(self: *Parser, left: Expression) !Expression {
        const expression_list = try self.parseExpressionList();
        if (expression_list.items.items.len != 1) {
            return error.InvalidIndexExpression;
        }

        const left_owned = try self.arena.create(Expression);
        left_owned.* = left;

        return Expression{
            .IndexExpression = IndexExpression{
                .left = left_owned,
                .index_expression = expression_list.items.items[0],
            },
        };
    }

    fn parseExpressionList(self: *Parser) !struct {
        items: std.ArrayListUnmanaged(*const Expression),
        delimiter: TokenType,
        is_one_liner: bool,
    } {
        const stopToken: TokenType = switch (self.cur_token.type) {
            .LBrace => .RBrace,
            .LBracket => .RBracket,
            .LParen => .RParen,
            else => return error.UnknownEnclosingToken,
        };

        try self.advance();

        var delimiter: TokenType = .NewLine;
        var items: std.ArrayListUnmanaged(*const Expression) = .{};
        const is_one_liner = !self.currentTokenIs(.NewLine);

        while (!self.currentTokenIs(stopToken)) {
            if (self.currentTokenIs(TokenType.Eof)) return ParserError.ReachedEndOfFile;

            try self.chopNewlines();

            if (self.currentTokenIs(stopToken)) break;

            try items.append(self.arena, try self.parseExpression(Precedence.Lowest));

            if (self.peekTokenIs(.Comma)) delimiter = .Comma;

            if (delimiter == .Comma) {
                try self.advance();
                if (self.currentTokenIs(stopToken)) break;

                const token_type = self.cur_token.type;
                try self.advance();
                try self.chopNewlines();
                if (!self.currentTokenIs(stopToken) and token_type != .Comma) {
                    return error.ExpectedComma;
                }

                if (self.currentTokenIs(stopToken)) break;

                continue;
            }

            try self.advance();
        }

        return .{ .items = items, .delimiter = delimiter, .is_one_liner = is_one_liner };
    }

    fn chopNewlines(self: *Parser) !void {
        while (self.currentTokenIs(.NewLine)) try self.advance();
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

    fn advance(self: *Parser) !void {
        self.cur_token = self.peek_token;
        self.peek_token = try self.lexer.next();
        // std.debug.print("cur token: {f}\n", .{self.cur_token});
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

    pub fn printProgram(program: std.ArrayList(*const Expression), arena: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(arena);
        for (program.items, 0..) |expression, i| {
            try std.fmt.format(buffer.writer(), "{f}", .{expression});
            if (i < program.items.len - 1) {
                try buffer.append('\n');
            }
        }
        return buffer.items;
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
            TokenType.Assign => Precedence.Index,
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
            TokenType.Assign => Operator.Assign,
            else => {
                std.debug.print("unknown operator: {any}\n", .{self.cur_token.type});
                return ParserError.NoOperatorFound;
            },
        };
    }
};
