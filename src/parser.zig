const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");

fn createAst(arena: std.mem.Allocator, source: []const u8) !Program {
    const parser = Parser.init(arena, source);
    return try parser.parseProgram();
}

pub const Parser = struct {
    current: scanner.Token,
    previous: ?scanner.Token,
    scanner: *scanner.Scanner,
    arena: std.mem.Allocator,
    debug_indent: usize = 0,

    pub fn init(arena: std.mem.Allocator, s: *scanner.Scanner) !Parser {
        const current = s.next();
        if (current.type == .Eof) {
            return error.NoTokens;
        }
        return Parser{
            .scanner = scanner,
            .current = current,
            .previous = null,
            .arena = arena,
        };
    }

    pub fn parseProgram(self: *Parser) !Program {
        var statements: std.ArrayList(Statement) = .{};
        while (self.cur_token.type != .Eof) {
            try self.chopNewlines();
            const statement = try self.parseStatement();
            try statements.append(self.arena, statement);
        }
        return statements;
    }

    fn parseStatement(self: *Parser) !void {
        if (try self.match(.Print)) {
            try self.printStatement();
        } else if (try self.match(.Assert)) {
            try self.assertStatement();
        } else if (try self.match(.Match)) {
            try self.matchStatement();
        } else if (try self.match(.While)) {
            try self.whileStatement();
        } else if (try self.match(.For)) {
            try self.forStatement();
        } else if (try self.match(.Return)) {
            try self.returnStatement();
        } else if (try self.match(.LBrace)) {
            try self.blockStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn printStatement(self: *Parser) !Statement {
        self.log("print statement", .{});
        defer self.log("end print statement", .{});

        try self.consume(.LParen, "Expect '(' after print.");
        const expression = try self.parseExpression();
        try self.expectLineEnd();

        return Statement{ .Print = expression };
    }

    fn assertStatement(self: *Parser) !Statement {
        self.log("assert statement", .{});
        defer self.log("end assert statement", .{});

        try self.consume(.LParen, "Expect '(' after print.");
        const expression = try self.parseExpression();
        try self.expectLineEnd();

        return Statement{ .Assert = expression };
    }

    fn matchStatement(self: *Parser) anyerror!void {
        self.log("match statement", .{});
        defer self.log("end match statement", .{});

        var has_target = false;

        if (self.check(.LParen)) {
            has_target = true;
            try self.parser.advance();
            try self.expression();
            try self.consume(.RParen, "Expect ')' after match target");
        }

        const instruction: chunk.OpCode = if (has_target) .JumpIfNotEq else .JumpIfFalse;

        if (!self.check(.LBrace)) {
            try self.expression();
            try self.consume(.Arrow, "Expect '->' after match expression");

            const then_jump = try self.emitJump(instruction);
            try self.statement();
            try self.patchJump(then_jump);
            try self.emitOpCode(.Pop);

            if (has_target) try self.emitOpCode(.Pop);

            return;
        }

        try self.parser.advance();
        try self.consume(.NewLine, "Expect new line after '{' in match block");

        var else_jumps: std.ArrayList(usize) = .{};
        errdefer else_jumps.deinit(self.gpa);

        while (!self.check(.Eof) and !self.check(.RBrace)) {
            try self.chopNewlines();

            if (self.check(.Else)) break;

            try self.expression();
            try self.consume(.Arrow, "Expect '->' after match expression");

            const then_jump = try self.emitJump(instruction);
            try self.emitOpCode(.Pop);
            try self.statement();
            try else_jumps.append(self.gpa, try self.emitJump(.Jump));
            try self.patchJump(then_jump);
            try self.emitOpCode(.Pop);
        }

        // TODO: what if the else branch is not the last?
        if (self.check(.Else)) {
            try self.parser.advance();
            try self.consume(.Arrow, "Expect '->' after else in match expression");
            try self.statement();
        }

        for (else_jumps.items) |jump| {
            try self.patchJump(jump);
        }

        if (has_target) try self.emitOpCode(.Pop);

        try self.consume(.RBrace, "Expect '}' at the end of match block");
        try self.consume(.NewLine, "Expect new line after match block");

        else_jumps.deinit(self.gpa);
    }

    fn expressionStatement(self: *Parser) !Statement {
        const expression = try self.parseExpression();
        if (self.match(.Assign)) {
            const target = switch (expression) {
                .Identifier => |name| AssignTarget{ .Identifier = name },
                .Index => |index| AssignTarget{ .Index = index },
            };
            const value = try self.parseExpression();
            try self.expectLineEnd();
            return Statement{ .Assignment = .{ .target = target, .expression = value } };
        }
        try self.expectLineEnd();
        return Statement{ .Expression = expression };
    }

    fn returnStatement(self: *Parser) !void {
        self.log("return statement", .{});
        defer self.log("end return statement", .{});

        if (self.type == .Script) {
            return self.errorAtPrevious("Can't return from top-level code.");
        }
        if (self.isLineEnd()) {
            try self.parser.advance();
            try self.emitReturn();
        } else {
            try self.expression();
            try self.expectLineEnd();
            try self.emitOpCode(.Return);
        }
    }

    fn whileStatement(self: *Parser) anyerror!void {
        self.log("while statement", .{});
        defer self.log("end while statement", .{});

        const loop_start = self.currentChunk().code.items.len;
        try self.consume(.LParen, "Expect '(' after 'while'");
        try self.expression();
        try self.consume(.RParen, "Expect ')' after condition");

        const exit_jump = try self.emitJump(.JumpIfFalse);
        try self.emitOpCode(.Pop);
        try self.statement();
        try self.emitLoop(loop_start);

        try self.patchJump(exit_jump);
        try self.emitOpCode(.Pop);
    }

    fn forStatement(self: *Parser) anyerror!void {
        self.log("for statement", .{});
        defer self.log("end for statement", .{});

        self.beginScope();

        try self.consume(.LParen, "Expect '(' after 'for'");
        try self.expression();
        try self.consume(.DotDot, "Expect '..' after expression");
        try self.expression();
        try self.consume(.RParen, "Expect ')' after range");

        try self.consume(.Pipe, "Expect '|' after loop range");
        try self.consume(.Identifier, "Expect identifier in loop capture");
        try self.declareVariable();
        self.markInitialized();
        try self.consume(.Pipe, "Expect '|' after variable capture");

        const increment_var_index = self.local_count - 1;

        // a dummy local for the right side of the range
        try self.addLocal(.{
            .source = "",
            .type = .Identifier,
            .start = 0,
            .length = 0,
            .line = 0,
        });
        self.markInitialized();

        const loop_start = self.currentChunk().code.items.len;

        const exit_jump = try self.emitJump(.JumpIfGreaterOrEq);
        try self.statement();

        try self.emitOpCode(.GetLocal);
        try self.emitU24(increment_var_index);
        try self.emitConstant(value.wrapInt(1));
        try self.emitOpCode(.Add);
        try self.emitOpCode(.SetLocal);
        try self.emitU24(increment_var_index);
        try self.emitOpCode(.Pop);

        try self.emitLoop(loop_start);

        try self.patchJump(exit_jump);

        try self.endScope();
    }

    fn blockStatement(self: *Compiler) anyerror!void {
        self.log("block statement", .{});
        defer self.log("end block statement", .{});

        while (!self.check(.RBrace) and !self.check(.Eof)) {
            try self.declaration();
        }
        try self.consume(.RBrace, "Expect '}' after block.");
        try self.consume(.NewLine, "Expect '\n' after block.");
    }

    fn printDebug(self: *Parser, comptime fmt: []const u8, args: anytype, color: logging.LogColor) void {
        logging.log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = .Debug,
            .color = color,
            .indent = self.debug_indent,
            .module = "Parser",
        });
    }

    fn consume(self: *Parser, expected: scanner.TokenType, message: []const u8) !void {
        if (self.check(expected)) {
            try self.parser.advance();
            return;
        }
        return self.errorAtCurrent(message);
    }

    fn match(self: *Parser, token_type: scanner.TokenType) !bool {
        if (!self.check(token_type)) return false;
        try self.advance();
        return true;
    }

    fn check(self: *Parser, token_type: scanner.TokenType) bool {
        return self.current.type == token_type;
    }

    fn isLineEnd(self: *Parser) bool {
        const token_type = self.current.type;
        return token_type == .NewLine or token_type == .Eof;
    }

    fn expectLineEnd(self: *Parser) !void {
        if (self.isLineEnd()) {
            try self.parser.advance();
            return;
        }
        return self.errorAtCurrent("Expected line end.");
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) anyerror {
        return errorAt(&self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) anyerror {
        return errorAt(&self.previous, message);
    }

    fn errorAt(token: *scanner.Token, message: []const u8) anyerror {
        std.debug.print("[line {d}] Error", .{token.line});
        switch (token.type) {
            .Eof => std.debug.print(" at end", .{}),
            .Error => {},
            else => std.debug.print(" at '{s}'", .{token.source[token.start .. token.start + token.length]}),
        }
        std.debug.print(": {s}\n", .{message});
        return error.CompileError;
    }

    fn parseExpression(self: *Parser) !*const Expression {
        return self.parsePrecedence(.Lowest);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !*const Expression {
        try self.parser.advance();

        self.log("expression on {s}", .{self.parser.previous.toString()});

        if (getRule(self.parser.previous.type).prefix) |prefixFn| {
            var left = try prefixFn(self);
            const left_owned = try self.arena.create(Expression);

            defer {
                left_owned.* = left;
                self.debug_indent -= 1;
                self.printDebug("parsed {s}\n", .{left_owned.getType()}, .Blue);
            }

            while (@intFromEnum(precedence) < getRulePrecedenceValue(self.parser.current.type)) {
                try self.advance();
                if (self.current.type == .Eof) return;
                if (getRule(self.parser.previous.type).infix) |infixFn| {
                    left = try infixFn(self, left);
                }
            }
        } else {
            return self.errorAtPrevious("Expect expression.");
        }

        self.log("end expression", .{});
    }

    fn parseVariable(self: *Parser, message: []const u8) !usize {
        try self.consume(.Identifier, message);
        self.current_var = self.parser.previous;

        try self.declareVariable();
        if (self.scope_depth > 0) return 0;

        return try self.identifierConstant(&self.parser.previous);
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
};

const Program = std.ArrayList(Statement);

const Statement = union(enum) {
    VarDeclaration: VarDeclaration,
    FuncDeclaration: FuncDeclaration,
    Block: Block,
    Assignment: Assignment,
    For: For,
    While: While,
    Return: *const Expression,
    Assert: *const Expression,
    Print: *const Expression,
    Expression: *const Expression,
};

const Block = std.ArrayList(Statement);

const VarDeclaration = struct {
    name: []const u8,
    expression: ?*const Expression,
};

const FuncDeclaration = struct {
    name: []const u8,
    expression: *const Function,
};

const Assignment = struct {
    target: AssignTarget,
    expression: *const Expression,
};

const AssignTarget = union(enum) {
    Identifier: []const u8,
    Index: Index,
};

const For = struct {
    range: Range,
    capture: []const u8,
    body: Block,
};

const While = struct {
    expression: *const Expression,
    body: Block,
};

const Expression = union(enum) {
    Identifier: []const u8,
    String: []const u8,
    Integer: i64,
    Float: f64,
    Boolean: bool,
    Infix: Infix,
    Prefix: Prefix,
    Function: Function,
    Call: Call,
    Range: Range,
    List: std.ArrayList(*const Expression),
    Table: std.ArrayList(TablePair),
    Index: Index,
    Match: Match,
    Null,
};

const Infix = struct {
    operator: scanner.TokenType,
    left: *const Expression,
    right: *const Expression,
};

const Prefix = struct {
    operator: scanner.TokenType,
    expression: *const Expression,
};

const Function = struct {
    params: std.ArrayList(FunctionParam),
    body: FunctionBody,
};

const FunctionBody = union(enum) {
    Block: Block,
    Expression: *const Expression,
};

const FunctionParam = union(enum) {
    Positional: []const u8,
    Default: struct { name: []const u8, value: *const Expression },
};

const Call = struct {
    function: *const Expression,
    args: std.ArrayList(FunctionArg),
};

const FunctionArg = union(enum) {
    Positional: *const Expression,
    Named: FunctionArgNamed,
};

const FunctionArgNamed = struct {
    name: []const u8,
    value: *const Expression,
};

const Range = struct {
    start: *const Expression,
    end: *const Expression,
};

const TablePair = struct {
    key: *const Expression,
    value: *const Expression,
};

const Index = struct {
    left: *const Expression,
    index: *const Expression,
};

const Match = struct {
    target: ?*const Expression,
    arms: std.ArrayList(MatchArm),
};

const MatchArm = struct {
    pattern: ?*const Expression,
    body: MatchArmBody,
};

const MatchArmBody = union(enum) {
    Block: Block,
    Expression: *const Expression,
};

const Precedence = enum(u8) {
    Lowest = 1,
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

fn parseString(parser: *Parser) !Expression {
    return Expression{ .String = parser.previous.?.toString() };
}

fn parseInteger(parser: *Parser) !Expression {
    return Expression{ .Integer = try std.fmt.parseInt(i64, parser.previous.?.toString(), 10) };
}

fn parseFloat(parser: *Parser) !Expression {
    return Expression{ .Float = try std.fmt.parseFloat(f64, parser.previous.?.toString()) };
}

fn parseLiteral(parser: *Parser) !Expression {
    return switch (parser.previous.type) {
        .True => try Expression{ .Boolean = true },
        .False => try Expression{ .Boolean = false },
        .Null => try Expression.Null,
        else => unreachable,
    };
}

fn parseIdentifier(parser: *Parser) !Expression {
    return Expression{ .Identifier = parser.previous.?.toString() };
}

fn parseUnary(parser: *Parser) !Expression {
    const operator = parser.previous.?.type;
    try parser.advance();
    const expression = try parser.parseExpression();
    return Expression{ .Prefix = .{ .operator = operator, .expression = expression } };
}

fn parseList(parser: *Parser) !Expression {
    _ = parser;
}

fn parseTable(parser: *Parser) !Expression {
    _ = parser;
}

fn parseFunction(parser: *Parser) !Expression {
    _ = parser;
}

fn parseGrouping(parser: *Parser) !Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RParen, "Expect ')' after expression");
    return expression;
}

fn parseBinary(parser: *Parser, left: Expression) !Expression {
    const operator = parser.previous.?.type;
    try parser.advance();
    const right = try parser.parsePrecedence(getRulePrecedence(operator));

    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;

    return Expression{
        .Infix = .{
            .operator = operator,
            .left = left_owned,
            .right = right,
        },
    };
}

fn parseDotDot(parser: *Parser, left: Expression) !Expression {
    try parser.advance();
    const right = try parser.parseExpression();
    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;
    return Expression{ .Range = .{ .left = left_owned, .right = right } };
}

fn parseCall(parser: *Parser, left: Expression) !Expression {
    var args: std.ArrayList(FunctionArg) = .{};
    while (true) {
        try parser.chopNewlines();
        if (try parser.match(.Eof)) return parser.errorAtCurrent("Reached EOF.");
        if (try parser.match(.RParen)) break;

        const expression = try parser.parseExpression();

        if (try parser.match(.Assign)) {
            if (expression != .Identifier) {
                return parser.errorAtPrevious("Invalid left side in named argument.");
            }
            const right = try parser.parseExpression();
            try args.append(parser.arena, FunctionArg{
                .Named = FunctionArgNamed{ .name = expression.Identifier, .value = right },
            });
        } else {
            try args.append(parser.arena, FunctionArg{ .Positional = expression });
        }

        try parser.advance();
        if (try parser.match(.RParen)) break;
        const token_type = parser.current.type;
        try parser.advance();
        try parser.chopNewlines();
        if (!parser.check(.RParen) and token_type != .Comma) {
            return parser.errorAtPrevious("Expected comma.");
        }
        if (try parser.match(.RParen)) break;
    }

    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;

    return Expression{ .Call = .{
        .function = left_owned,
        .args = args,
    } };
}

fn parseIndex(parser: *Parser, left: Expression) !Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RBracket, "Expect ']' after index expression.");

    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;

    return Expression{
        .Index = .{
            .left = left_owned,
            .index = expression,
        },
    };
}

const ParseRule = struct {
    prefix: ?*const fn (parser: *Parser) anyerror!Expression,
    infix: ?*const fn (parser: *Parser, left: Expression) anyerror!Expression,
    precedence: ?Precedence,
};

const token_count = @typeInfo(scanner.TokenType).@"enum".fields.len;

const rules: [token_count]ParseRule = initRules();

fn initRules() [token_count]ParseRule {
    var table: [token_count]ParseRule = undefined;

    inline for (@typeInfo(scanner.TokenType).@"enum".fields) |field| {
        const index = field.value;
        const tag = @as(scanner.TokenType, @enumFromInt(index));
        table[index] = switch (tag) {
            .LParen => .{ .prefix = parseGrouping, .infix = parseCall, .precedence = .Call },
            .RParen => .{ .prefix = null, .infix = null, .precedence = null },
            .LBracket => .{ .prefix = null, .infix = parseIndex, .precedence = .Index },
            .RBracket => .{ .prefix = null, .infix = null, .precedence = null },
            .LBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .RBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .Assign => .{ .prefix = null, .infix = null, .precedence = .Assign },
            .Comma => .{ .prefix = null, .infix = null, .precedence = null },
            .String => .{ .prefix = parseString, .infix = null, .precedence = null },
            .Float => .{ .prefix = parseFloat, .infix = null, .precedence = null },
            .Int => .{ .prefix = parseInteger, .infix = null, .precedence = null },
            .True => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
            .False => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
            .Null => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
            .Identifier => .{ .prefix = parseIdentifier, .infix = null, .precedence = null },
            .Bang => .{ .prefix = parseUnary, .infix = null, .precedence = null },
            .Lt => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .LtOrEq => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .Gt => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .GtOrEq => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .Eq => .{ .prefix = null, .infix = parseBinary, .precedence = .Equals },
            .NotEq => .{ .prefix = null, .infix = parseBinary, .precedence = .Equals },
            .Plus => .{ .prefix = null, .infix = parseBinary, .precedence = .Sum },
            .Minus => .{ .prefix = parseUnary, .infix = parseBinary, .precedence = .Sum },
            .Slash => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Asterisk => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Percent => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Return => .{ .prefix = null, .infix = null, .precedence = null },
            .Else => .{ .prefix = null, .infix = null, .precedence = null },
            .For => .{ .prefix = null, .infix = null, .precedence = null },
            .Dot => .{ .prefix = null, .infix = null, .precedence = .Index },
            .DotDot => .{ .prefix = null, .infix = parseDotDot, .precedence = null },
            .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
            .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
            .Eof => .{ .prefix = null, .infix = null, .precedence = null },
            .And => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalAnd },
            .Or => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalOr },
            .Pipe => .{ .prefix = null, .infix = null, .precedence = .BitwiseOr },
            .Ampersand => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseAnd },
            .Caret => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseXor },
            .Tilde => .{ .prefix = null, .infix = parseBinary, .precedence = .Prefix },
            .LeftShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .RightShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .Print => .{ .prefix = null, .infix = null, .precedence = null },
            .Assert => .{ .prefix = null, .infix = null, .precedence = null },
            .Let => .{ .prefix = null, .infix = null, .precedence = null },
            .Match => .{ .prefix = null, .infix = null, .precedence = null },
            .While => .{ .prefix = null, .infix = null, .precedence = null },
            .Fn => .{ .prefix = parseFunction, .infix = null, .precedence = null },
            .List => .{ .prefix = parseList, .infix = null, .precedence = null },
            .Table => .{ .prefix = parseTable, .infix = null, .precedence = null },
            .Error => .{ .prefix = null, .infix = null, .precedence = null },
        };
    }

    return table;
}

fn getRule(token_type: scanner.TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

fn getRulePrecedenceValue(token_type: scanner.TokenType) usize {
    return @intFromEnum(getRulePrecedence(token_type));
}

fn getRulePrecedence(token_type: scanner.TokenType) Precedence {
    if (getRule(token_type).precedence) |precedence| {
        return precedence;
    }
    return Precedence.Lowest;
}
