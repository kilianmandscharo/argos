const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");
const ast = @import("ast.zig");
const constants = @import("constants.zig");

pub fn createAst(arena: std.mem.Allocator, source: []const u8) !ast.Program {
    var scan = scanner.Scanner.init(source);
    var parser = try Parser.init(arena, &scan);
    return try parser.parseProgram();
}

pub const Parser = struct {
    current: scanner.Token,
    previous: ?scanner.Token,
    scanner: *scanner.Scanner,
    arena: std.mem.Allocator,
    debug_indent: usize = 0,
    ctx: struct {
        parse_loop_capture: bool,
        current_var_name: ?[]const u8,
    },

    pub fn init(arena: std.mem.Allocator, s: *scanner.Scanner) !Parser {
        const current = try s.next();
        if (current.type == .Eof) {
            return error.NoTokens;
        }
        return Parser{
            .scanner = s,
            .current = current,
            .previous = null,
            .arena = arena,
            .ctx = .{
                .parse_loop_capture = false,
                .current_var_name = null,
            },
        };
    }

    fn log(self: *Parser, comptime fmt: []const u8, args: anytype) void {
        logging.log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = .Debug,
            .indent = self.debug_indent,
            .module = "Parser",
        });
    }

    pub fn parseProgram(self: *Parser) !ast.Program {
        var statements: std.ArrayList(ast.Statement) = .{};
        while (self.current.type != .Eof) {
            try self.chopNewlines();
            const statement = try self.parseStatement();
            try statements.append(self.arena, statement);
        }
        return statements;
    }

    fn parseStatement(self: *Parser) anyerror!ast.Statement {
        try self.chopNewlines();
        if (try self.match(.Print)) {
            return try self.printStatement();
        }
        if (try self.match(.Assert)) {
            return try self.assertStatement();
        }
        if (try self.match(.While)) {
            return try self.whileStatement();
        }
        if (try self.match(.For)) {
            return try self.forStatement();
        }
        if (try self.match(.Return)) {
            return try self.returnStatement();
        }
        if (try self.match(.LBrace)) {
            return try self.blockStatement();
        }
        if (try self.match(.Let)) {
            return try self.varDeclaration();
        }
        return try self.expressionStatement();
    }

    fn printStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) {
            self.log("print statement", .{});
            defer self.log("end print statement", .{});
        }

        try self.consume(.LParen, "Expect '(' after print.");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after print expression.");

        _ = try self.match(.NewLine);

        return .{ .Print = expression };
    }

    fn assertStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) {
            self.log("assert statement", .{});
            defer self.log("end assert statement", .{});
        }

        try self.consume(.LParen, "Expect '(' after assert.");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after assert expression.");

        _ = try self.match(.NewLine);

        return .{ .Assert = expression };
    }

    fn whileStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) {
            self.log("while statement", .{});
            defer self.log("end while statement", .{});
        }

        try self.consume(.LParen, "Expect '(' after 'while'.");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after condition.");
        // TODO: allow all statements here
        try self.consume(.LBrace, "Expect '{' after while condition.");
        const body = try self.blockStatement();

        return .{
            .While = .{ .expression = expression, .body = body.Block },
        };
    }

    fn forStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) {
            self.log("for statement", .{});
            defer self.log("end for statement", .{});
        }

        try self.consume(.LParen, "Expect '(' after 'for'.");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after range.");

        try self.consume(.Pipe, "Expect '|' after loop range.");
        self.ctx.parse_loop_capture = true;

        const capture = try self.parseExpression();
        if (capture.* != .Identifier) {
            return self.errorAtPrevious("Expect identifier in loop capture.");
        }

        var index: ?[]const u8 = null;

        if (!self.check(.Pipe)) {
            if (!try self.match(.Comma)) {
                return self.errorAtCurrent("Expected comma.");
            }

            const second_capture = try self.parseExpression();
            if (second_capture.* != .Identifier) {
                return self.errorAtPrevious("Expect identifier in loop capture.");
            }
            index = second_capture.Identifier;
        }

        try self.consume(.Pipe, "Expect '|' after for loop capture.");
        self.ctx.parse_loop_capture = false;

        // TODO: allow all statements here
        try self.consume(.LBrace, "Expect '{' after loop capture.");
        const body = try self.blockStatement();

        return .{
            .For = .{
                .expression = expression,
                .capture = capture.Identifier,
                .index = index,
                .body = body.Block,
            },
        };
    }

    fn returnStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) {
            self.log("return statement", .{});
            defer self.log("end return statement", .{});
        }

        if (self.isLineEnd()) {
            const expression = try self.arena.create(ast.Expression);
            expression.* = .{ .Null = {} };
            return .{ .Return = expression };
        } else {
            const expression = try self.parseExpression();
            try self.expectLineEnd();
            return .{ .Return = expression };
        }
    }

    fn blockStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) {
            self.log("block statement", .{});
            defer self.log("end block statement", .{});
        }

        try self.chopNewlines();
        var statements: std.ArrayList(ast.Statement) = .{};
        while (!self.check(.RBrace) and !self.check(.Eof)) {
            try statements.append(self.arena, try self.parseStatement());
        }
        try self.consume(.RBrace, "Expect '}' after block.");
        try self.expectLineEnd();

        return .{ .Block = statements };
    }

    fn varDeclaration(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) {
            self.log("var declaration", .{});
            defer self.log("end var declaration", .{});
        }

        const target = try self.parseExpression();
        if (target.* != .Identifier) {
            return self.errorAtPrevious("Expect identifier after 'let'.");
        }

        self.ctx.current_var_name = target.Identifier;
        defer self.ctx.current_var_name = null;

        if (try self.match(.Assign)) {
            const value = try self.parseExpression();
            return .{
                .VarDeclaration = .{
                    .name = target.Identifier,
                    .expression = value,
                },
            };
        }

        try self.expectLineEnd();
        const nullExpression = try self.arena.create(ast.Expression);
        nullExpression.* = .Null;
        return .{
            .VarDeclaration = .{
                .name = target.Identifier,
                .expression = nullExpression,
            },
        };
    }

    fn expressionStatement(self: *Parser) !ast.Statement {
        const expression = try self.parseExpression();
        if (try self.match(.Assign)) {
            const target: ast.AssignTarget = switch (expression.*) {
                .Identifier => |name| .{ .Identifier = name },
                .Index => |index| .{ .Index = index },
                else => return self.errorAtPrevious("Invalid assign target."),
            };
            const value = try self.parseExpression();
            try self.expectLineEnd();
            return .{ .Assignment = .{ .target = target, .expression = value } };
        }
        try self.expectLineEnd();
        return .{ .Expression = expression };
    }

    fn parseExpression(self: *Parser) !*const ast.Expression {
        return self.parsePrecedence(.Lowest);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !*const ast.Expression {
        try self.advance();

        if (comptime constants.debug_parser) {
            self.log("expression on {s}", .{self.previous.?.toString()});
            self.debug_indent += 1;
        }

        if (getRule(self.previous.?.type).prefix) |prefixFn| {
            var left = try prefixFn(self);
            const left_owned = try self.arena.create(ast.Expression);

            defer {
                left_owned.* = left;
                if (comptime constants.debug_parser) {
                    self.debug_indent -= 1;
                    self.log("parsed {s}", .{@tagName(left_owned.*)});
                }
            }

            while (@intFromEnum(precedence) < getRulePrecedenceValue(self, self.current.type)) {
                try self.advance();
                if (self.current.type == .Eof) break;
                if (getRule(self.previous.?.type).infix) |infixFn| {
                    left = try infixFn(self, left);
                }
            }

            return left_owned;
        } else {
            return self.errorAtPrevious("Expect expression.");
        }
    }

    fn parseCommaSeparated(self: *Parser, T: type, parseFn: *const fn (p: *Parser) anyerror!T, delimiter: scanner.TokenType) !std.ArrayList(T) {
        var items: std.ArrayList(T) = .{};
        var expectComma = false;

        while (!try self.match(delimiter)) {
            try self.chopNewlines();

            if (try self.match(.Eof)) return self.errorAtCurrent("Reached EOF.");
            if (try self.match(delimiter)) break;

            if (expectComma) return self.errorAtPrevious("Expected comma.");

            try items.append(self.arena, try parseFn(self));
            if (!try self.match(.Comma)) expectComma = true;
        }

        return items;
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;
        self.current = try self.scanner.next();
    }

    fn chopNewlines(self: *Parser) !void {
        while (self.check(.NewLine)) {
            try self.advance();
        }
    }

    fn consume(self: *Parser, expected: scanner.TokenType, message: []const u8) !void {
        if (self.check(expected)) {
            try self.advance();
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
            try self.advance();
            return;
        }
        return self.errorAtCurrent("Expected line end.");
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) anyerror {
        return errorAt(&self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) anyerror {
        return errorAt(&self.previous.?, message);
    }

    fn errorAt(token: *scanner.Token, message: []const u8) anyerror {
        std.debug.print("[line {d}] Error", .{token.line});
        switch (token.type) {
            .Eof => std.debug.print(" at end", .{}),
            else => std.debug.print(" at '{s}'", .{token.toString()}),
        }
        std.debug.print(": {s}\n", .{message});
        return error.ParserError;
    }
};

const Precedence = enum(u8) {
    Lowest = 1,
    Range,
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

fn parseString(parser: *Parser) !ast.Expression {
    const s = parser.previous.?.toString();
    return .{ .String = s[1 .. s.len - 1] };
}

fn parseInteger(parser: *Parser) !ast.Expression {
    return .{ .Integer = try std.fmt.parseInt(i64, parser.previous.?.toString(), 10) };
}

fn parseFloat(parser: *Parser) !ast.Expression {
    return .{ .Float = try std.fmt.parseFloat(f64, parser.previous.?.toString()) };
}

fn parseLiteral(parser: *Parser) !ast.Expression {
    return switch (parser.previous.?.type) {
        .True => .{ .Boolean = true },
        .False => .{ .Boolean = false },
        .Null => .{ .Null = {} },
        else => unreachable,
    };
}

fn parseIdentifier(parser: *Parser) !ast.Expression {
    return .{ .Identifier = parser.previous.?.toString() };
}

fn parseUnary(parser: *Parser) !ast.Expression {
    const operator = parser.previous.?.type;
    const expression = try parser.parseExpression();
    return .{ .Prefix = .{ .operator = operator, .expression = expression } };
}

fn parseList(parser: *Parser) !ast.Expression {
    try parser.consume(.LBrace, "Expect '{' after 'List'.");
    const items = try parser.parseCommaSeparated(
        *const ast.Expression,
        struct {
            fn parse(p: *Parser) !*const ast.Expression {
                return try p.parseExpression();
            }
        }.parse,
        .RBrace,
    );
    return .{ .List = items };
}

fn parseTable(parser: *Parser) !ast.Expression {
    try parser.consume(.LBrace, "Expect '{' after 'Table'.");
    const items = try parser.parseCommaSeparated(
        ast.TablePair,
        struct {
            fn parse(p: *Parser) !ast.TablePair {
                const key = try p.parseExpression();
                try p.consume(.Assign, "Expect '=' after table key.");
                const value = try p.parseExpression();
                return ast.TablePair{ .key = key, .value = value };
            }
        }.parse,
        .RBrace,
    );
    return .{ .Table = items };
}

fn parseFunction(parser: *Parser) !ast.Expression {
    try parser.consume(.LParen, "Expect '(' after 'fn'.");
    const params = try parser.parseCommaSeparated(
        ast.FunctionParam,
        struct {
            fn parse(p: *Parser) !ast.FunctionParam {
                const expression = try p.parseExpression();
                if (try p.match(.Assign)) {
                    if (expression.* != .Identifier) {
                        return p.errorAtPrevious("Invalid left side in default param.");
                    }
                    const right = try p.parseExpression();
                    return .{
                        .Default = .{ .name = expression.Identifier, .value = right },
                    };
                } else {
                    if (expression.* != .Identifier) {
                        return p.errorAtPrevious("Invalid function param.");
                    }
                    return .{ .Positional = expression.Identifier };
                }
            }
        }.parse,
        .RParen,
    );

    const name = parser.ctx.current_var_name;

    if (try parser.match(.LBrace)) {
        const body = try parser.blockStatement();
        return .{
            .Function = .{
                .params = params,
                .body = .{ .Block = body.Block },
                .name = name,
            },
        };
    }

    const body = try parser.parseExpression();
    return .{
        .Function = .{
            .params = params,
            .body = .{ .Expression = body },
            .name = name,
        },
    };
}

fn parseGrouping(parser: *Parser) !ast.Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RParen, "Expect ')' after expression.");
    return expression.*;
}

fn parseMatch(self: *Parser) !ast.Expression {
    var target: ?*const ast.Expression = null;

    if (try self.match(.LParen)) {
        target = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after match target.");
    }

    if (!self.check(.LBrace)) {
        const pattern = try self.parseExpression();
        try self.consume(.Arrow, "Expect '->' after match pattern.");
        const body = try self.parseStatement();
        return .{
            .Match = .{
                .target = target,
                .body = .{
                    .Single = .{ .pattern = pattern, .body = body },
                },
            },
        };
    }

    try self.advance();
    try self.consume(.NewLine, "Expect new line after '{' in match block.");

    var arms: std.ArrayList(ast.MatchArm) = .{};

    while (!self.check(.Eof) and !self.check(.RBrace)) {
        try self.chopNewlines();

        const pattern = try self.parseExpression();
        try self.consume(.Arrow, "Expect '->' after match pattern.");
        const body = try self.parseStatement();

        try arms.append(self.arena, .{ .pattern = pattern, .body = body });
    }

    try self.consume(.RBrace, "Expect '}' at the end of match block.");
    try self.expectLineEnd();

    return .{
        .Match = .{ .target = target, .body = .{ .Multiple = arms } },
    };
}

fn parseBinary(parser: *Parser, left: ast.Expression) !ast.Expression {
    const operator = parser.previous.?.type;
    const right = try parser.parsePrecedence(getRulePrecedence(parser, operator));

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .{
        .Infix = .{
            .operator = operator,
            .left = left_owned,
            .right = right,
        },
    };
}

fn parseDotDot(parser: *Parser, left: ast.Expression) !ast.Expression {
    const right = try parser.parseExpression();
    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;
    return .{ .Range = .{ .start = left_owned, .end = right } };
}

fn parseCall(parser: *Parser, left: ast.Expression) !ast.Expression {
    const args = try parser.parseCommaSeparated(
        ast.FunctionArg,
        struct {
            fn parse(p: *Parser) !ast.FunctionArg {
                const expression = try p.parseExpression();
                if (try p.match(.Assign)) {
                    if (expression.* != .Identifier) {
                        return p.errorAtPrevious("Invalid left side in named argument.");
                    }
                    const right = try p.parseExpression();
                    return .{
                        .Named = .{ .name = expression.Identifier, .value = right },
                    };
                } else {
                    return .{ .Positional = expression };
                }
            }
        }.parse,
        .RParen,
    );

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .{
        .Call = .{
            .function = left_owned,
            .args = args,
        },
    };
}

fn parseIndex(parser: *Parser, left: ast.Expression) !ast.Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RBracket, "Expect ']' after index expression.");

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .{
        .Index = .{
            .left = left_owned,
            .index = expression,
        },
    };
}

const ParseRule = struct {
    prefix: ?*const fn (parser: *Parser) anyerror!ast.Expression,
    infix: ?*const fn (parser: *Parser, left: ast.Expression) anyerror!ast.Expression,
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
            .Assign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
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
            .Plus => .{ .prefix = parseUnary, .infix = parseBinary, .precedence = .Sum },
            .Minus => .{ .prefix = parseUnary, .infix = parseBinary, .precedence = .Sum },
            .Slash => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Asterisk => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Percent => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Return => .{ .prefix = null, .infix = null, .precedence = null },
            .For => .{ .prefix = null, .infix = null, .precedence = null },
            .Dot => .{ .prefix = null, .infix = null, .precedence = .Index },
            .DotDot => .{ .prefix = null, .infix = parseDotDot, .precedence = .Range },
            .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
            .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
            .Eof => .{ .prefix = null, .infix = null, .precedence = null },
            .And => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalAnd },
            .Or => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalOr },
            .Pipe => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseOr },
            .Ampersand => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseAnd },
            .Caret => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseXor },
            .Tilde => .{ .prefix = parseUnary, .infix = null, .precedence = .Prefix },
            .LeftShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .RightShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .Print => .{ .prefix = null, .infix = null, .precedence = null },
            .Assert => .{ .prefix = null, .infix = null, .precedence = null },
            .Let => .{ .prefix = null, .infix = null, .precedence = null },
            .Match => .{ .prefix = parseMatch, .infix = null, .precedence = null },
            .While => .{ .prefix = null, .infix = null, .precedence = null },
            .Fn => .{ .prefix = parseFunction, .infix = null, .precedence = null },
            .List => .{ .prefix = parseList, .infix = null, .precedence = null },
            .Table => .{ .prefix = parseTable, .infix = null, .precedence = null },
        };
    }

    return table;
}

fn getRule(token_type: scanner.TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

fn getRulePrecedenceValue(parser: *Parser, token_type: scanner.TokenType) usize {
    return @intFromEnum(getRulePrecedence(parser, token_type));
}

fn getRulePrecedence(parser: *Parser, token_type: scanner.TokenType) Precedence {
    if (token_type == .Pipe and parser.ctx.parse_loop_capture) return .Lowest;
    if (getRule(token_type).precedence) |precedence| {
        return precedence;
    }
    return Precedence.Lowest;
}
