const std = @import("std");

const logging = @import("logging.zig");
const LogColor = logging.LogColor;
const log = logging.log;

const lexer_module = @import("lexer.zig");
const Token = lexer_module.Token;
const TokenType = lexer_module.TokenType;
const Lexer = lexer_module.Lexer;

pub const FunctionParam = union(enum) {
    Identifier: []const u8,
    AssignmentExpression: AssignmentExpression,

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        switch (self) {
            .Identifier => |v| try writer.print("{s}", .{v}),
            .AssignmentExpression => |v| try writer.print("{f}", .{v}),
        }
    }
};

pub const Expression = union(enum) {
    Program: std.ArrayList(*const Expression),
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
    ArrayLiteral: std.ArrayList(*const Expression),
    TableLiteral: std.ArrayList(*const Expression),
    BlockExpression: BlockExpression,
    AssignmentExpression: AssignmentExpression,
    ReturnExpression: *const Expression,
    IndexExpression: IndexExpression,
    Statement: *const Expression,
    Null,

    pub fn getType(self: @This()) []const u8 {
        return switch (self) {
            .Program => "Program",
            .Identifier => "Identifier",
            .StringLiteral => "StringLiteral",
            .IntegerLiteral => "IntegerLiteral",
            .FloatLiteral => "FloatLiteral",
            .BooleanLiteral => "BooleanLiteral",
            .InfixExpression => "InfixExpression",
            .PrefixExpression => "PrefixExpression",
            .FunctionLiteral => "FunctionLiteral",
            .CallExpression => "CallExpression",
            .IfExpression => "IfExpression",
            .RangeExpression => "RangeExpression",
            .ForExpression => "ForExpression",
            .ArrayLiteral => "ArrayLiteral",
            .TableLiteral => "TableLiteral",
            .BlockExpression => "BlockExpression",
            .AssignmentExpression => "AssignmentExpression",
            .ReturnExpression => "ReturnExpression",
            .IndexExpression => "IndexExpression",
            .Statement => "Statement",
            .Null => "Null",
        };
    }

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        switch (self) {
            .Program => |v| {
                for (v.items) |item| {
                    try item.format(writer);
                }
            },
            .Identifier => |v| try writer.print("{s}", .{v}),
            .StringLiteral => |v| try writer.print("\"{s}\"", .{v}),
            .FloatLiteral => |v| try writer.print("{}", .{v}),
            .IntegerLiteral => |v| try writer.print("{}", .{v}),
            .BooleanLiteral => |v| try writer.print("{}", .{v}),
            .InfixExpression => |v| try writer.print("({f} {f} {f})", .{ v.left, v.operator, v.right }),
            .PrefixExpression => |v| try writer.print("({f}{f})", .{ v.operator, v.expression }),
            .FunctionLiteral => |v| {
                try writer.print("(", .{});
                for (v.params.items, 0..) |param, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{f}", .{param});
                }
                try writer.print(") -> ", .{});
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
            .IndexExpression => |index_expression| {
                try writer.print("{f}[{f}]", .{ index_expression.left, index_expression.index_expression });
            },
            .BlockExpression => |v| try v.format(writer),
            .AssignmentExpression => |v| try v.format(writer),
            .ReturnExpression => |v| try writer.print("(return {f})", .{v}),
            .Statement => |v| try writer.print("({f})", .{v}),
            .Null => try writer.print("null", .{}),
        }
    }
};

pub const Operator = enum {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Gt,
    GtOrEq,
    Lt,
    LtOrEq,
    Eq,
    NotEq,
    Bang,
    Assign,
    And,
    Or,
    Pipe,
    Ampersand,
    Caret,
    Tilde,
    LeftShift,
    RightShift,
    Percent,

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        switch (self) {
            .Plus => try writer.print("+", .{}),
            .Minus => try writer.print("-", .{}),
            .Asterisk => try writer.print("*", .{}),
            .Slash => try writer.print("/", .{}),
            .Gt => try writer.print(">", .{}),
            .Lt => try writer.print("<", .{}),
            .Eq => try writer.print("==", .{}),
            .NotEq => try writer.print("!=", .{}),
            .Bang => try writer.print("!", .{}),
            .Assign => try writer.print("=", .{}),
            .GtOrEq => try writer.print(">=", .{}),
            .LtOrEq => try writer.print("<=", .{}),
            .And => try writer.print("and", .{}),
            .Or => try writer.print("or", .{}),
            .Pipe => try writer.print("|", .{}),
            .Ampersand => try writer.print("&", .{}),
            .Caret => try writer.print("^", .{}),
            .Tilde => try writer.print("~", .{}),
            .LeftShift => try writer.print("<<", .{}),
            .RightShift => try writer.print(">>", .{}),
            .Percent => try writer.print("%", .{}),
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
    params: std.ArrayList(FunctionParam),
    body: *const Expression,
};

pub const CallExpression = struct {
    function: *const Expression,
    args: std.ArrayList(*const Expression),
};

pub const IfExpression = struct {
    condition: *const Expression,
    body: BlockExpression,
    alternative: ?BlockExpression,
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
    expressions: std.ArrayList(*const Expression),

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (self.expressions.items.len == 0) {
            try writer.print("{{}}", .{});
            return;
        }
        try writer.print("{{\n", .{});
        for (self.expressions.items) |expression| {
            try writer.print("    {f}\n", .{expression});
        }
        try writer.print("}}", .{});
    }
};

pub const AssignmentExpression = struct {
    left: *const Expression,
    expression: *const Expression,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("({f} = {f})", .{ self.left, self.expression });
    }
};

pub const IndexExpression = struct {
    left: *const Expression,
    index_expression: *const Expression,
};

const Precedence = enum(u8) {
    Lowest = 1,
    Range,
    Assign,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equals,
    LessGreater,
    Shift,
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
    debug: bool = false,
    debug_indent: usize = 0,

    pub fn init(lexer: *Lexer, options: struct { arena: std.mem.Allocator, debug: bool = false }) !Parser {
        const cur_token = try lexer.next();
        if (cur_token.type == .Eof) {
            return ParserError.NoTokens;
        }
        const peek_token = try lexer.next();
        return Parser{
            .lexer = lexer,
            .cur_token = cur_token,
            .peek_token = peek_token,
            .arena = options.arena,
            .debug = options.debug,
        };
    }

    fn printDebug(self: *Parser, comptime fmt: []const u8, args: anytype, color: LogColor) void {
        log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = if (self.debug) .Debug else .Fatal,
            .color = color,
            .indent = self.debug_indent,
            .module = "Parser",
        });
    }

    pub fn parseProgram(self: *Parser) !Expression {
        var list: std.ArrayList(*const Expression) = .{};
        while (self.cur_token.type != .Eof) {
            self.printDebug("parse statement on {f}\n", .{self.cur_token}, .None);
            const expression = try self.parseExpression(.Lowest);
            self.printDebug("parsed statement {s}\n", .{expression.getType()}, .None);
            const wrapper = Expression{ .Statement = expression };
            const owned = try self.arena.create(Expression);
            owned.* = wrapper;
            try self.advance();
            try list.append(self.arena, owned);
            while (!self.currentTokenIs(.Eof)) {
                if (self.currentTokenIs(.NewLine)) {
                    try self.advance();
                } else {
                    break;
                }
            }
        }
        return Expression{ .Program = list };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) anyerror!*Expression {
        self.printDebug("parse expression on {f}\n", .{self.cur_token}, .Blue);
        self.debug_indent += 1;

        self.printDebug("parse left on {f}\n", .{self.cur_token}, .Green);
        var left = try self.parseLeft();
        self.printDebug("parsed left {s}\n", .{left.getType()}, .Green);
        const left_owned = try self.arena.create(Expression);

        defer {
            left_owned.* = left;
            self.debug_indent -= 1;
            self.printDebug("parsed {s}\n", .{left_owned.getType()}, .Blue);
        }

        while (self.peek_token.type != .NewLine and @intFromEnum(precedence) < @intFromEnum(self.getPeekPrecedence())) {
            try self.advance();
            self.printDebug("parse right on {f}\n", .{self.cur_token}, .Green);
            left = self.parseRight(left) catch |err| {
                switch (err) {
                    ParserError.NoInfixFunctionFound => return left_owned,
                    else => return err,
                }
            };
            self.printDebug("parsed right {s}\n", .{left.getType()}, .Green);
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
            .Bang, .Plus, .Minus, .Tilde => try self.parsePrefix(),
            .If => try self.parseIf(),
            .For => try self.parseFor(),
            .LBracket => try self.parseBracket(),
            .LBrace => try self.parseBrace(),
            .Return => try self.parseReturn(),
            .LParen => try self.parseParen(),
            .Null => self.parseNull(),
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

    fn parseFunction(self: *Parser, params: std.ArrayList(*const Expression)) !Expression {
        // TODO: the params list should be freed immediately after transformation,
        // this requires reworking the memory management strategy of the parser
        var final_params: std.ArrayList(FunctionParam) = .{};
        for (params.items) |expression| {
            switch (expression.*) {
                .Identifier => |val| try final_params.append(self.arena, FunctionParam{ .Identifier = val }),
                .AssignmentExpression => |val| try final_params.append(self.arena, FunctionParam{ .AssignmentExpression = val }),
                else => return error.InvalidFunctionParam,
            }
        }

        const body = try self.arena.create(Expression);

        if (self.currentTokenIs(.LBrace)) {
            const result = try self.parseExpressionList(null);
            body.* = Expression{ .BlockExpression = BlockExpression{ .expressions = result.items } };
        } else {
            const expression = try self.parseExpression(.Lowest);
            body.* = expression.*;
        }

        return Expression{
            .FunctionLiteral = FunctionLiteral{
                .params = final_params,
                .body = body,
            },
        };
    }

    fn parseIf(self: *Parser) !Expression {
        try self.advance();

        const condition = try self.parseExpression(.Lowest);

        try self.advanceAndExpect(.LBrace);

        const result = try self.parseExpressionList(null);

        var if_expression = Expression{
            .IfExpression = .{
                .condition = condition,
                .body = BlockExpression{
                    .expressions = result.items,
                },
                .alternative = null,
            },
        };

        try self.advance();

        if (!self.currentTokenIs(.Else)) {
            if (!self.currentTokenIs(.Eof)) {
                try self.expectToken(.NewLine);
            }
            return if_expression;
        }

        try self.advanceAndExpect(.LBrace);

        const alternative = try self.parseExpressionList(null);
        if_expression.IfExpression.alternative = BlockExpression{
            .expressions = alternative.items,
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

        const body = try self.parseExpressionList(null);

        return Expression{
            .ForExpression = ForExpression{
                .variable = variable.Identifier,
                .range = range.RangeExpression,
                .body = BlockExpression{
                    .expressions = body.items,
                },
            },
        };
    }

    fn parseBracket(self: *Parser) !Expression {
        const result = try self.parseExpressionList(null);
        return Expression{
            .ArrayLiteral = result.items,
        };
    }

    fn parseBrace(self: *Parser) !Expression {
        const expression_list = try self.parseExpressionList(.Comma);

        const expression = switch (expression_list.delimiter) {
            .Comma => comma: {
                const expr = Expression{
                    .TableLiteral = expression_list.items,
                };
                for (expression_list.items.items) |item| {
                    if (item.* != .AssignmentExpression) {
                        return error.ExpectedAssignment;
                    }
                }
                break :comma expr;
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

    fn parseParen(self: *Parser) !Expression {
        const result = try self.parseExpressionList(.Comma);
        if (self.peekTokenIs(.Arrow)) {
            try self.advance();
            try self.advance();
            return try self.parseFunction(result.items);
        }
        if (result.items.items.len == 0) return error.NoExpressionsInParen;
        return result.items.items[0].*;
    }

    fn parseNull(self: *Parser) Expression {
        _ = self.cur_token.literal;
        return .Null;
    }

    fn parseRight(self: *Parser, left: Expression) !Expression {
        return switch (self.cur_token.type) {
            .Plus,
            .Minus,
            .Slash,
            .Asterisk,
            .Eq,
            .NotEq,
            .Lt,
            .Gt,
            .LtOrEq,
            .GtOrEq,
            .LeftShift,
            .RightShift,
            .Pipe,
            .Ampersand,
            .Caret,
            .Or,
            .And,
            .Percent,
            => self.parseInfix(left),
            .LParen => self.parseCall(left),
            .DotDot => self.parseRange(left),
            .Assign => self.parseAssign(left),
            .LBracket => self.parseIndex(left),
            .Dot => self.parseDot(left),
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
        const result = try self.parseExpressionList(null);

        var keyword_arg_found = false;
        for (result.items.items) |arg| {
            switch (arg.*) {
                .AssignmentExpression => {
                    keyword_arg_found = true;
                },
                else => {
                    if (keyword_arg_found) return error.PositionalAfterKeywordArg;
                },
            }
        }

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
        const left_owned = try self.arena.create(Expression);
        left_owned.* = left;
        return Expression{
            .AssignmentExpression = AssignmentExpression{
                .left = left_owned,
                .expression = expression,
            },
        };
    }

    fn parseIndex(self: *Parser, left: Expression) !Expression {
        const expression_list = try self.parseExpressionList(null);
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

    fn parseDot(self: *Parser, left: Expression) !Expression {
        try self.advance();
        const expression = try self.parseExpression(.Index);

        if (expression.* != .Identifier) return error.InvalidDotIndexExpression;

        const right_owned = try self.arena.create(Expression);
        right_owned.* = Expression{ .StringLiteral = expression.Identifier };

        const left_owned = try self.arena.create(Expression);
        left_owned.* = left;

        return Expression{
            .IndexExpression = IndexExpression{
                .left = left_owned,
                .index_expression = right_owned,
            },
        };
    }

    fn parseExpressionList(self: *Parser, defaultDelimiter: ?TokenType) !struct {
        items: std.ArrayList(*const Expression),
        delimiter: TokenType,
    } {
        self.printDebug("parse expression list on {f}\n", .{self.cur_token}, .None);

        const stopToken: TokenType = switch (self.cur_token.type) {
            .LBrace => .RBrace,
            .LBracket => .RBracket,
            .LParen => .RParen,
            else => return error.UnknownEnclosingToken,
        };

        var delimiter: TokenType = defaultDelimiter orelse switch (self.cur_token.type) {
            .LBrace => .NewLine,
            .LBracket => .Comma,
            .LParen => .Comma,
            else => return error.UnknownEnclosingToken,
        };

        try self.advance();

        var items: std.ArrayList(*const Expression) = .{};

        while (!self.currentTokenIs(stopToken)) {
            if (self.currentTokenIs(.Eof)) return ParserError.ReachedEndOfFile;

            try self.chopNewlines();

            if (self.currentTokenIs(stopToken)) break;

            self.printDebug("parse expression list item on {f}\n", .{self.cur_token}, .None);
            const expression = try self.parseExpression(Precedence.Lowest);
            self.printDebug("parsed expression list item {s}\n", .{expression.getType()}, .None);
            try items.append(self.arena, expression);

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

        if (delimiter == .NewLine) {
            for (items.items, 0..) |item, i| {
                const statement = try self.arena.create(Expression);
                statement.* = Expression{ .Statement = item };
                items.items[i] = statement;
            }
        }

        self.printDebug("parsed expression list\n", .{}, .None);

        return .{ .items = items, .delimiter = delimiter };
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
        self.printDebug("advanced to {f}\n", .{self.cur_token}, .None);
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
            .DotDot => .Range,
            .Assign => .Assign,
            .Or => .LogicalOr,
            .And => .LogicalAnd,
            .Pipe => .BitwiseOr,
            .Caret => .BitwiseXor,
            .Ampersand => .BitwiseAnd,
            .Eq => .Equals,
            .NotEq => .Equals,
            .Lt => .LessGreater,
            .Gt => .LessGreater,
            .GtOrEq => .LessGreater,
            .LtOrEq => .LessGreater,
            .LeftShift => .Shift,
            .RightShift => .Shift,
            .Plus => .Sum,
            .Minus => .Sum,
            .Slash => .Product,
            .Asterisk => .Product,
            .Percent => .Product,
            .Tilde => .Prefix,
            .LParen => .Call,
            .LBracket => .Index,
            .Dot => .Index,
            else => .Lowest,
        };
    }

    fn getCurrentOperator(self: *Parser) !Operator {
        return switch (self.cur_token.type) {
            .Plus => .Plus,
            .Minus => .Minus,
            .Asterisk => .Asterisk,
            .Slash => .Slash,
            .Gt => .Gt,
            .Lt => .Lt,
            .Eq => .Eq,
            .NotEq => .NotEq,
            .Bang => .Bang,
            .Assign => .Assign,
            .GtOrEq => .GtOrEq,
            .LtOrEq => .LtOrEq,
            .And => .And,
            .Or => .Or,
            .Pipe => .Pipe,
            .Ampersand => .Ampersand,
            .Caret => .Caret,
            .Tilde => .Tilde,
            .LeftShift => .LeftShift,
            .RightShift => .RightShift,
            .Percent => .Percent,
            else => {
                std.debug.print("unknown operator: {any}\n", .{self.cur_token.type});
                return ParserError.NoOperatorFound;
            },
        };
    }
};
