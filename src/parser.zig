const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");
const ast = @import("ast.zig");
const constants = @import("constants.zig");
const vm = @import("vm.zig");

pub fn createAst(arena: std.mem.Allocator, script_context: *vm.ScriptContext) !*const ast.Expression {
    var s = try scanner.Scanner.init(arena, script_context);
    var p = try Parser.init(arena, &s, script_context);
    return try p.parseProgram();
}

pub const Parser = struct {
    current: scanner.Token,
    previous: ?scanner.Token,
    scanner: *scanner.Scanner,
    arena: std.mem.Allocator,
    debug_indent: usize = 0,
    script_context: *vm.ScriptContext,
    ctx: struct {
        current_var_name: ?[]const u8,
    },

    pub fn init(arena: std.mem.Allocator, s: *scanner.Scanner, script_context: *vm.ScriptContext) !Parser {
        const current = try s.next();
        if (current.type == .Eof) {
            return error.NoTokens;
        }
        return Parser{
            .script_context = script_context,
            .scanner = s,
            .current = current,
            .previous = null,
            .arena = arena,
            .ctx = .{
                .current_var_name = null,
            },
        };
    }

    fn getPrevious(self: *Parser) scanner.Token {
        return self.previous.?;
    }

    fn log(self: *Parser, comptime fmt: []const u8, args: anytype) void {
        logging.log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = .Debug,
            .indent = self.debug_indent,
            .module = "Parser",
        });
    }

    pub fn parseProgram(self: *Parser) !*const ast.Expression {
        const grouping = try self.arena.create(ast.Expression);
        grouping.* = .{
            .data = .{ .Grouping = .empty },
            .token = scanner.Token.dummy(),
        };

        const program = try self.arena.create(ast.Expression);
        program.* = .{
            .data = .{
                .Function = .{
                    .name = "Program",
                    .params = .empty,
                    .body = grouping,
                },
            },
            .token = scanner.Token.dummy(),
        };

        while (self.current.type != .Eof) {
            try self.chopNewlines();
            if (self.current.type == .Eof) break;
            try grouping.data.Grouping.append(self.arena, try self.parseExpression());
        }

        return program;
    }

    fn parseExpression(self: *Parser) !*const ast.Expression {
        return self.parsePrecedence(.Lowest);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !*const ast.Expression {
        try self.advance();

        if (comptime constants.debug_parser) {
            self.log("expression on {s}", .{self.getPrevious().toString()});
            self.debug_indent += 1;
        }

        if (getRule(self.getPrevious().type).prefix) |prefixFn| {
            var left = try prefixFn(self);
            const left_owned = try self.arena.create(ast.Expression);

            defer {
                left_owned.* = left;
                if (comptime constants.debug_parser) {
                    self.debug_indent -= 1;
                    self.log("parsed {s}", .{@tagName(left_owned.data)});
                }
            }

            while (@backingInt(precedence) < getRulePrecedenceValue(self.current.type)) {
                try self.advance();
                if (self.current.type == .Eof) break;
                const t = self.getPrevious().type;
                if (getRule(t).infix) |infixFn| {
                    if (comptime constants.debug_parser) {
                        self.log("infix on {s}", .{@tagName(t)});
                    }
                    left = try infixFn(self, left);
                }
            }

            return left_owned;
        } else {
            return self.errorAtPrevious("expect expression");
        }
    }

    fn parseSeparated(self: *Parser, T: type, parseFn: *const fn (p: *Parser) anyerror!T, delimiter: scanner.TokenType, end_of_line: ?scanner.TokenType) !std.ArrayList(T) {
        var items: std.ArrayList(T) = .empty;
        var expect_line_end_separator = false;
        var line_end_separator: ?scanner.TokenType = end_of_line;

        while (!try self.match(delimiter)) {
            try self.chopNewlines();

            if (try self.match(.Eof)) return self.errorAtCurrent("Reached EOF.");
            if (try self.match(delimiter)) break;

            if (expect_line_end_separator) return self.errorAtPrevious("expected line end separator");

            try items.append(self.arena, try parseFn(self));

            if (line_end_separator == null) {
                if (self.check(.Comma)) line_end_separator = .Comma;
                if (self.check(.NewLine)) line_end_separator = .NewLine;
            }

            if (line_end_separator) |sep| {
                if (!try self.match(sep)) expect_line_end_separator = true;
            }
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

    fn matchLineEnd(self: *Parser) !void {
        if (self.isLineEnd()) {
            try self.advance();
        }
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) anyerror {
        return self.errorAt(self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) anyerror {
        return self.errorAt(self.getPrevious(), message);
    }

    fn errorAt(self: *Parser, token: scanner.Token, message: []const u8) anyerror {
        token.printError(message, self.script_context, "Parser");
        return error.ParserError;
    }
};

const Precedence = enum(u8) {
    Lowest = 1,
    Assign,
    LogicalOr,
    LogicalAnd,
    Equals,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    LessGreater,
    Shift,
    Sum,
    Product,
    Prefix,
    Call,
};

fn parseString(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const s = token.toString();
    return .init(.{ .String = s[1 .. s.len - 1] }, token);
}

fn parseInteger(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    return .init(.{ .Integer = try std.fmt.parseInt(i64, token.toString(), 10) }, token);
}

fn parseFloat(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    return .init(.{ .Float = try std.fmt.parseFloat(f64, token.toString()) }, token);
}

fn parseLiteral(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const data: ast.ExpressionData = switch (token.type) {
        .True => .{ .Boolean = true },
        .False => .{ .Boolean = false },
        else => unreachable,
    };
    return .init(data, token);
}

fn parseIdentifier(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    return .init(.{ .Identifier = token.toString() }, token);
}

fn parseUnary(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const operator = token.type;
    const right = try parser.parseExpression();
    return .init(.{ .Unary = .{ .operator = operator, .right = right } }, token);
}

fn parseList(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const items = try parser.parseSeparated(
        *const ast.Expression,
        struct {
            fn parse(p: *Parser) !*const ast.Expression {
                return try p.parseExpression();
            }
        }.parse,
        .RBracket,
        .Comma,
    );
    return .init(.{ .List = items }, token);
}

fn parseGrouping(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();

    if (try parser.match(.RParen)) {
        if (try parser.match(.Arrow)) {
            const body = try parser.parseExpression();
            return .init(.{ .Function = .{ .params = .empty, .body = body, .name = parser.ctx.current_var_name } }, token);
        }
        return .init(.{ .Unit = {} }, token);
    }

    const expressions = try parser.parseSeparated(
        *const ast.Expression,
        struct {
            fn parse(p: *Parser) !*const ast.Expression {
                return try p.parseExpression();
            }
        }.parse,
        .RParen,
        null,
    );

    if (try parser.match(.Arrow)) {
        const body = try parser.parseExpression();

        var params: std.ArrayList(ast.FunctionParam) = .empty;
        for (expressions.items) |expr| {
            if (expr.data == .Declaration) {
                const name = expr.data.Declaration.left;
                const value = expr.data.Declaration.right;
                try params.append(parser.arena, .{ .Default = .{ .name = name, .value = value } });
            } else if (expr.data == .Identifier) {
                try params.append(parser.arena, .{ .Positional = expr.token });
            } else {
                return parser.errorAtPrevious("invalid function parameter");
            }
        }

        return .init(.{ .Function = .{ .params = params, .body = body, .name = parser.ctx.current_var_name } }, token);
    }

    if (expressions.items.len == 1) {
        return expressions.items[0].*;
    }

    return .init(.{ .Grouping = expressions }, token);
}

fn parseMatch(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const target = try parser.parseExpression();

    try parser.consume(.With, "expect 'with' after match target");

    var arms: std.ArrayList(ast.MatchArm) = .empty;

    while (true) {
        try parser.chopNewlines();

        if (!try parser.match(.Pipe)) break;

        const pattern = try parser.parseExpression();
        try parser.consume(.Arrow, "expect '->' after match pattern");
        const expression = try parser.parseExpression();

        try arms.append(parser.arena, .{ .pattern = pattern, .expression = expression });
    }

    return .init(.{ .Match = .{ .target = target, .arms = arms } }, token);
}

fn parseIf(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();

    var data: ast.ExpressionData = .{ .If = .{ .arms = .empty, .alt = null } };

    while (true) {
        try parser.chopNewlines();

        if (data.If.arms.items.len == 0) {
            const condition = try parser.parseExpression();
            try parser.consume(.Then, "expect 'then' after if target expression");
            try parser.chopNewlines();
            try data.If.arms.append(parser.arena, .{ .condition = condition, .then = try parser.parseExpression() });
            continue;
        }

        if (try parser.match(.Elif)) {
            const condition = try parser.parseExpression();
            try parser.consume(.Then, "expect 'then' after elif target expression");
            try parser.chopNewlines();
            try data.If.arms.append(parser.arena, .{ .condition = condition, .then = try parser.parseExpression() });
            continue;
        }

        if (try parser.match(.Else)) {
            try parser.chopNewlines();
            const alt = try parser.parseExpression();
            data.If.alt = alt;
        }

        break;
    }

    return .init(data, token);
}

fn parseBinary(parser: *Parser, left: ast.Expression) !ast.Expression {
    const operator = parser.getPrevious().type;
    const right = try parser.parsePrecedence(getRulePrecedence(operator));

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .init(.{ .Binary = .{ .operator = operator, .left = left_owned, .right = right } }, left.token);
}

fn parseCall(parser: *Parser, left: ast.Expression) !ast.Expression {
    const args = try parser.parseSeparated(
        *const ast.Expression,
        struct {
            fn parse(p: *Parser) !*const ast.Expression {
                return try p.parseExpression();
            }
        }.parse,
        .RParen,
        .Comma,
    );

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .init(.{ .Call = .{ .function = left_owned, .args = args } }, left.token);
}

fn parseDeclaration(parser: *Parser, left: ast.Expression) !ast.Expression {
    if (left.data != .Identifier) {
        return parser.errorAtPrevious("expect identifier on left side of declaration");
    }

    parser.ctx.current_var_name = left.data.Identifier;
    defer parser.ctx.current_var_name = null;

    const right = try parser.parseExpression();

    return .init(.{ .Declaration = .{ .left = left.token, .right = right } }, left.token);
}

const ParseRule = struct {
    prefix: ?*const fn (parser: *Parser) anyerror!ast.Expression,
    infix: ?*const fn (parser: *Parser, left: ast.Expression) anyerror!ast.Expression,
    precedence: ?Precedence,
};

fn getRule(token_type: scanner.TokenType) ParseRule {
    return switch (token_type) {
        .LParen => .{ .prefix = parseGrouping, .infix = parseCall, .precedence = .Call },
        .RParen => .{ .prefix = null, .infix = null, .precedence = null },
        .LBracket => .{ .prefix = parseList, .infix = null, .precedence = null },
        .RBracket => .{ .prefix = null, .infix = null, .precedence = null },
        .Assign => .{ .prefix = null, .infix = parseDeclaration, .precedence = .Assign },
        .Comma => .{ .prefix = null, .infix = null, .precedence = null },
        .String => .{ .prefix = parseString, .infix = null, .precedence = null },
        .Float => .{ .prefix = parseFloat, .infix = null, .precedence = null },
        .Int => .{ .prefix = parseInteger, .infix = null, .precedence = null },
        .True => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
        .False => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
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
        .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
        .And => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalAnd },
        .Or => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalOr },
        .Pipe => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseOr },
        .Ampersand => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseAnd },
        .Caret => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseXor },
        .Tilde => .{ .prefix = parseUnary, .infix = null, .precedence = .Prefix },
        .LeftShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
        .RightShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
        .Match => .{ .prefix = parseMatch, .infix = null, .precedence = null },
        .With => .{ .prefix = null, .infix = null, .precedence = null },
        .If => .{ .prefix = parseIf, .infix = null, .precedence = null },
        .Then => .{ .prefix = null, .infix = null, .precedence = null },
        .Elif => .{ .prefix = null, .infix = null, .precedence = null },
        .Else => .{ .prefix = null, .infix = null, .precedence = null },
        .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
        .Eof => .{ .prefix = null, .infix = null, .precedence = null },
    };
}

fn getRulePrecedenceValue(token_type: scanner.TokenType) usize {
    return @backingInt(getRulePrecedence(token_type));
}

fn getRulePrecedence(token_type: scanner.TokenType) Precedence {
    return getRule(token_type).precedence orelse .Lowest;
}
